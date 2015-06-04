/* notmuch - Not much of an email program, (just index and search)
 *
 * Copyright © 2009 Carl Worth
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see http://www.gnu.org/licenses/ .
 *
 * Author: Carl Worth <cworth@cworth.org>
 */

#include "notmuch-client.h"
#include "hex-escape.h"
#include "string-util.h"
#include <zlib.h>


static int
database_dump_file (notmuch_database_t *notmuch, gzFile output,
		    const char *query_str, int output_format)
{
    notmuch_query_t *query;
    notmuch_messages_t *messages;
    notmuch_message_t *message;
    notmuch_tags_t *tags;

    if (! query_str)
	query_str = "";

    query = notmuch_query_create (notmuch, query_str);
    if (query == NULL) {
	fprintf (stderr, "Out of memory\n");
	return EXIT_FAILURE;
    }
    /* Don't ask xapian to sort by Message-ID. Xapian optimizes returning the
     * first results quickly at the expense of total time.
     */
    notmuch_query_set_sort (query, NOTMUCH_SORT_UNSORTED);

    char *buffer = NULL;
    size_t buffer_size = 0;

    for (messages = notmuch_query_search_messages (query);
	 notmuch_messages_valid (messages);
	 notmuch_messages_move_to_next (messages)) {
	int first = 1;
	const char *message_id;

	message = notmuch_messages_get (messages);
	message_id = notmuch_message_get_message_id (message);

	if (output_format == DUMP_FORMAT_BATCH_TAG &&
	    strchr (message_id, '\n')) {
	    /* This will produce a line break in the output, which
	     * would be difficult to handle in tools.  However, it's
	     * also impossible to produce an email containing a line
	     * break in a message ID because of unfolding, so we can
	     * safely disallow it. */
	    fprintf (stderr, "Warning: skipping message id containing line break: \"%s\"\n", message_id);
	    notmuch_message_destroy (message);
	    continue;
	}

	if (output_format == DUMP_FORMAT_SUP) {
	    gzprintf (output, "%s (", message_id);
	}

	for (tags = notmuch_message_get_tags (message);
	     notmuch_tags_valid (tags);
	     notmuch_tags_move_to_next (tags)) {
	    const char *tag_str = notmuch_tags_get (tags);

	    if (! first)
		gzputs (output, " ");

	    first = 0;

	    if (output_format == DUMP_FORMAT_SUP) {
		gzputs (output, tag_str);
	    } else {
		if (hex_encode (notmuch, tag_str,
				&buffer, &buffer_size) != HEX_SUCCESS) {
		    fprintf (stderr, "Error: failed to hex-encode tag %s\n",
			     tag_str);
		    return EXIT_FAILURE;
		}
		gzprintf (output, "+%s", buffer);
	    }
	}

	if (output_format == DUMP_FORMAT_SUP) {
	    gzputs (output, ")\n");
	} else {
	    if (make_boolean_term (notmuch, "id", message_id,
				   &buffer, &buffer_size)) {
		    fprintf (stderr, "Error quoting message id %s: %s\n",
			     message_id, strerror (errno));
		    return EXIT_FAILURE;
	    }
	    gzprintf (output, " -- %s\n", buffer);
	}

	notmuch_message_destroy (message);
    }

    notmuch_query_destroy (query);

    return EXIT_SUCCESS;
}

/* Dump database into output_file_name if it's non-NULL, stdout
 * otherwise.
 */
int
notmuch_database_dump (notmuch_database_t *notmuch,
		       const char *output_file_name,
		       const char *query_str,
		       dump_format_t output_format,
		       notmuch_bool_t gzip_output)
{
    gzFile output = NULL;
    const char *mode = gzip_output ? "w9" : "wT";
    const char *name_for_error = output_file_name ? output_file_name : "stdout";

    char *tempname = NULL;
    int outfd = -1;

    int ret = -1;

    if (output_file_name) {
	tempname = talloc_asprintf (notmuch, "%s.XXXXXX", output_file_name);
	outfd = mkstemp (tempname);
    } else {
	outfd = dup (STDOUT_FILENO);
    }

    if (outfd < 0) {
	fprintf (stderr, "Bad output file %s\n", name_for_error);
	goto DONE;
    }

    output = gzdopen (outfd, mode);

    if (output == NULL) {
	fprintf (stderr, "Error opening %s for (gzip) writing: %s\n",
		 name_for_error, strerror (errno));
	if (close (outfd))
	    fprintf (stderr, "Error closing %s during shutdown: %s\n",
		 name_for_error, strerror (errno));
	goto DONE;
    }

    ret = database_dump_file (notmuch, output, query_str, output_format);
    if (ret) goto DONE;

    ret = gzflush (output, Z_FINISH);
    if (ret) {
	fprintf (stderr, "Error flushing output: %s\n", gzerror (output, NULL));
	goto DONE;
    }

    if (output_file_name) {
	ret = fsync (outfd);
	if (ret) {
	    fprintf (stderr, "Error syncing %s to disk: %s\n",
		     name_for_error, strerror (errno));
	    goto DONE;
	}
    }

    if (gzclose_w (output) != Z_OK) {
	fprintf (stderr, "Error closing %s: %s\n", name_for_error,
		 gzerror (output, NULL));
	ret = EXIT_FAILURE;
	output = NULL;
	goto DONE;
    }

    if (output_file_name) {
	ret = rename (tempname, output_file_name);
	if (ret) {
	    fprintf (stderr, "Error renaming %s to %s: %s\n",
		     tempname, output_file_name, strerror (errno));
	    goto DONE;
	}

    }
 DONE:
    if (ret != EXIT_SUCCESS && output)
	(void) gzclose_w (output);

    if (ret != EXIT_SUCCESS && output_file_name)
	(void) unlink (tempname);

    return ret;
}

int
notmuch_dump_command (notmuch_config_t *config, int argc, char *argv[])
{
    notmuch_database_t *notmuch;
    const char *query_str = NULL;
    int ret;

    if (notmuch_database_open (notmuch_config_get_database_path (config),
			       NOTMUCH_DATABASE_MODE_READ_WRITE, &notmuch))
	return EXIT_FAILURE;

    char *output_file_name = NULL;
    int opt_index;

    int output_format = DUMP_FORMAT_BATCH_TAG;
    notmuch_bool_t gzip_output = 0;

    notmuch_opt_desc_t options[] = {
	{ NOTMUCH_OPT_KEYWORD, &output_format, "format", 'f',
	  (notmuch_keyword_t []){ { "sup", DUMP_FORMAT_SUP },
				  { "batch-tag", DUMP_FORMAT_BATCH_TAG },
				  { 0, 0 } } },
	{ NOTMUCH_OPT_STRING, &output_file_name, "output", 'o', 0  },
	{ NOTMUCH_OPT_BOOLEAN, &gzip_output, "gzip", 'z', 0 },
	{ NOTMUCH_OPT_INHERIT, (void *) &notmuch_shared_options, NULL, 0, 0 },
	{ 0, 0, 0, 0, 0 }
    };

    opt_index = parse_arguments (argc, argv, options, 1);
    if (opt_index < 0)
	return EXIT_FAILURE;

    notmuch_process_shared_options (argv[0]);

    if (opt_index < argc) {
	query_str = query_string_from_args (notmuch, argc - opt_index, argv + opt_index);
	if (query_str == NULL) {
	    fprintf (stderr, "Out of memory.\n");
	    return EXIT_FAILURE;
	}
    }

    ret = notmuch_database_dump (notmuch, output_file_name, query_str,
				 output_format, gzip_output);

    notmuch_database_destroy (notmuch);

    return ret;
}

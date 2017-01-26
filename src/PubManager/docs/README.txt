PubManager
===========

PubManager is a module in the TRIPS system which downloads and chunks PubMed Central documents, primarily those in OpenAccess format.


1. Command line options:

	-cache /path/to/cache # where cached files (downloaded and processed) get stored

PubManager will use (and create, if necessary) two subfolders inside the cache folder:

	xml/ # where downloaded XML files are stored
	ppp/ # where processed files are stored

The tool uses the cache to store results so that subsequent requests on the same article can pull the results directly from the cache instead of re-downloading and re-processing it again.

2. Messages

receives:

	(request :content (pub-pull :pmcid ID) :reply-with R01)

sends:

  1. on success

	(reply :content (done :output-folder "/path/to/output/pmcid”) :in-reply-to R01)

  2. on failure

	(reply :content (failure :msg “error message") :in-reply-to R01)


3. Testing using the test-client binary:

./bin/PubManager -cache ./pubMed -debug true

./bin/trips_client
(register :name test-client)
(tell :content (module-status ready))
(request :content (pub-pull :pmcid 5042122) :reply-with RO1)
(request :content (pub-pull :pmcid 3667761) :reply-with RO2)


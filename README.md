# GAdsLib
It's a pascal library wrapping the REST Interface of Google Ads API.

Its current state include a mix of languages (portuguese and english), some boiler plate and a few adaptations to fit the business needs in the context it was created.

It's a VCL Project but will eventually (after fork) become a package (or not).

Currently holds strong dependencies on the FireDac Library.

# To-Do

[ ] - Documentation.
[ ] - Create a Routine for DB resources creation (such as queries, connections, etc).
[ ] - Interact with the products view from the reporting aspect of the API.
[ ] - Create custom library exceptions and swap all ``Exception.Create()`` calls with meaningful, contextualized, exceptions.
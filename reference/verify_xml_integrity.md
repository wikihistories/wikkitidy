# Check that a Wikimedia XML file has not been corrupted

The Wikimedia Foundation publishes MD5 checksums for all its database
dumps. This function looks up the published sha1 checksums based on the
file name, then compares them to the locally calcualte has using the
`openssl` package.

## Usage

``` r
verify_xml_integrity(path)
```

## Arguments

- path:

  The path to the file

## Value

True (invisibly) if successful, otherwise error

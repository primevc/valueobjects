CREATE KEYSPACE fs WITH replication = {
    'class': 'SimpleStrategy',
    'replication_factor': 3
};

CREATE TABLE fs.files (
    hash blob PRIMARY KEY,
    data blob
);

CREATE KEYSPACE fs WITH replication = {
    'class': 'SimpleStrategy',
    'replication_factor': 3
};

CREATE TABLE fs.files (
    hash varchar PRIMARY KEY,
    data blob
);

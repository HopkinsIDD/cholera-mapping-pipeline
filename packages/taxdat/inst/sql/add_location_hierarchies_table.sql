CREATE TABLE IF NOT EXISTS location_hierarchies(ancestor_id bigint REFERENCES locations(id), descendant_id bigint REFERENCES locations(id), generations integer);

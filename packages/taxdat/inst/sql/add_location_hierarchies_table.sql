CREATE TABLE location_hierarchies(ancestor_id bigint REFERENCES locations(id), descendant_id bigint REFERENCES locations(id), generations integer);

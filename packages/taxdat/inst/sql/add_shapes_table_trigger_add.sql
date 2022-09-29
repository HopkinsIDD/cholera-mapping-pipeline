CREATE TRIGGER shapes_boundary_and_valid_default
BEFORE INSERT ON shapes
FOR EACH ROW
EXECUTE PROCEDURE trigger_shapes_insert_boundary_and_valid();

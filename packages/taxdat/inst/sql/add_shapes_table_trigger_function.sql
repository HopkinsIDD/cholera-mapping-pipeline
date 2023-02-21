create or replace function trigger_shapes_insert_boundary_and_valid()
  RETURNS TRIGGER
  LANGUAGE plpgsql AS
  $func$
  BEGIN
  NEW.boundary = st_boundary(NEW.shape);
  NEW.valid = st_isvalid(NEW.shape);
  return NEW;
  END
  $func$;

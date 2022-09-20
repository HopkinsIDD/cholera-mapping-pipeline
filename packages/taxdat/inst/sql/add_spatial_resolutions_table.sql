CREATE TABLE IF NOT EXISTS grids.spatial_resolutions(width_in_km int, height_in_km int, UNIQUE(width_in_km, height_in_km));

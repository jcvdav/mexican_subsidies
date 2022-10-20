# Get total number of large-scale fishing vessels (n = 3093)
SELECT
  COUNT(DISTINCT(vessel_rnpa))
FROM
  `emlab-gcp.mex_fisheries.vessel_info_v_20220912`
WHERE fleet = "large scale"

# Get number of dedicated shrimp trawlers (that don't fish for anything else) (n = 1559)
SELECT
  COUNT(DISTINCT(vessel_rnpa))
FROM
  `emlab-gcp.mex_fisheries.vessel_info_v_20220912`
WHERE fleet = "large scale"
AND shrimp = 1
AND tuna = 0
AND sardine = 0
AND others = 0

# How many vessels in VMS? (n = 4198)
SELECT
  COUNT(DISTINCT(vessel_rnpa))
FROM
  `emlab-gcp.mex_fisheries.mex_vms_v_20220912`

# Temporal coverage of VMS? (January 2008 to January 2022)
SELECT
  year,
  month,
  COUNT(*)
FROM
  `emlab-gcp.mex_fisheries.mex_vms_v_20220912`
GROUP BY year, month
ORDER BY year, month


# How many matched to large-scale registry? (n = 2394)
SELECT
  COUNT(DISTINCT(vessel_rnpa))
FROM
  `emlab-gcp.mex_fisheries.mex_vms_v_20220912`
INNER JOIN (
  SELECT
    DISTINCT(vessel_rnpa)
  FROM
    `emlab-gcp.mex_fisheries.vessel_info_v_20220912`
  WHERE
    fleet = "large scale")
USING
  (vessel_rnpa)

# How many matched to large-scale dedicated shrimp trawlers? (n = 1256)
SELECT
  COUNT(DISTINCT(vessel_rnpa))
FROM
  `emlab-gcp.mex_fisheries.mex_vms_v_20220912`
INNER JOIN (
  SELECT
    DISTINCT(vessel_rnpa)
  FROM
    `emlab-gcp.mex_fisheries.vessel_info_v_20220912`
  WHERE
    fleet = "large scale"
  AND shrimp = 1
  AND tuna = 0
  AND sardine = 0
  AND others = 0
    )
USING
  (vessel_rnpa)
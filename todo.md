# To Do

### Stratified sampling

We have studies identified by study_uid. The studies have a manufacturer, one of 1,2,3,4 and ethnic groups drawn
from A, B, C, D, E. We want to sample n studies from each manufacturer with the ethnic groups balanced as evenly as possible.

Claude says this:

1. Within each (manufacturer, ethnic_group), assign a random rank to each study.
2. Then "interleave" the ethnic groups by assigning a global rank within each manufacturer that cycles through ethnic groups. This ensures you pull evenly from each group before going back for seconds.

```sql
WITH ranked_within_group AS (
    -- Randomly order studies within each (manufacturer, ethnic_group)
    SELECT
        study_uid,
        manufacturer,
        ethnic_group,
        ROW_NUMBER() OVER (
            PARTITION BY manufacturer, ethnic_group
            ORDER BY RANDOM()
        ) AS rn_within_group
    FROM studies
),

interleaved AS (
    -- Now assign a global rank per manufacturer that round-robins
    -- across ethnic groups. The trick: sort by rn_within_group first
    -- (the "round" number), then by ethnic_group (to break ties
    -- deterministically within a round).
    SELECT
        study_uid,
        manufacturer,
        ethnic_group,
        ROW_NUMBER() OVER (
            PARTITION BY manufacturer
            ORDER BY rn_within_group, HASH(study_uid || rn_within_group)
        ) AS global_rank
    FROM ranked_within_group
)

SELECT *
FROM interleaved
WHERE global_rank <= :n  -- your desired sample size per manufacturer
ORDER BY manufacturer, global_rank;
```

This makes the "leftover" slots in a partial round random rather than always favoring group A.

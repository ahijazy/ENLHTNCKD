CREATE TABLE #Codesets (
  codeset_id int NOT NULL,
  concept_id bigint NOT NULL
)
;

INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 0 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
  select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (2615855,2615854,2615853,2615856,2001524,2001522,2001523,2001520,2001521,2001518,2313813,2313812,2725537,2725536,2725538,2725531,43533354,2725530,2725533,2725532,2725535,2725534,4231493,43533186,43533187,43533242,43533223,43528000,43528001,43528002,43528003,43528004,4184832,4283892,4006788,42894478,2725929,42894479,2725928,2725931,42894476,42894477,2725930,42894474,2725933,2725932,42894475,42894472,2725935,42894473,2725934,2725921,42894470,2725920,42894471,2725923,42894468,2725922,42894469,2725925,2725924,42894467,2725927,2725926,2725945,2725944,2725947,2725946,2725949,2725948,2725951,2725950,2725937,2725936,42894484,2725939,2725938,42894482,2725941,42894483,2725940,42894480,2725943,2725942,42894481,2725897,2725896,2725899,2725898,2725901,2725900,2725903,2725902,2725913,2725912,2725915,2725914,2725917,2725916,2725919,2725918,2725905,2725904,2725907,2725906,2725909,2725908,2725911,2725910,2725481,2725480,2725483,2725482,2725484,2725473,2725472,2725475,2725474,2725477,2725476,2725479,2725478,2725961,2725960,2725963,2725962,44789455,2725965,2725964,2725967,2725966,2725953,2313803,2725952,2313802,2725955,2313801,2725954,2725957,2725956,2725959,2313804,2725958,2725465,42894590,2725464,42894591,42894588,2725467,2725466,42894589,42894586,2725469,42894587,2725468,2725471,42894584,2725470,42894585,2725969,2725968,2725970,43527908,43527909,43533352,43533353,43533247,4031850,43533248,42894606,42894607,42894604,42894605,42894602,42894603,42894600,42894601,42894598,42894599,42894596,42894597,42894594,42894595,42894592,42894593,42894622,42894623,42894620,42894621,42894618,42894619,42894616,42894617,42894614,42894615,42894612,4137380,42894613,42894610,42894611,42894608,42894609,43527994,43527995,43527996,43527997,43527998,42894632,42894633,43527999,42894630,42894631,42894628,42894629,42894626,42894627,42894624,42894625,42894654,42894655,42894652,42894653,42894650,42894651,42894648,42894649,42894646,42894647,42894644,42894645,42894643,42894670,42894671,42894668,42894669,42894666,42894667,42894664,42894665,42894662,42894663,42894660,42894661,42894658,42894659,42894656,42894657,42894686,42894687,42894684,42894685,42894682,42894683,42894680,42894681,42894678,42894679,42894676,42894677,42894674,42894675,42894672,42894673,42894702,42894703,42894700,42894701,42894698,42894699,42894696,42894697,42894694,42894695,42894692,42894693,42894690,42894691,42894688,42894689,42894718,42894716,42894717,42894714,42894715,42894712,42894713,42894710,42894711,42894708,42894709,42894706,42894707,42894704,42894705))
UNION  select c.concept_id
  from @vocabulary_database_schema.CONCEPT c
  join @vocabulary_database_schema.CONCEPT_ANCESTOR ca on c.concept_id = ca.descendant_concept_id
  WHERE c.invalid_reason is null
  and (ca.ancestor_concept_id in (2615855,2615854,2615853,2615856,2001524,2001522,2001523,2001520,2001521,2001518,2313813,2313812,2725537,2725536,2725538,2725531,43533354,2725530,2725533,2725532,2725535,2725534,4231493,43533186,43533187,43533242,43533223,43528000,43528001,43528002,43528003,43528004,42894478,2725929,42894479,2725928,2725931,42894476,42894477,2725930,42894474,2725933,2725932,42894475,42894472,2725935,42894473,2725934,2725921,42894470,2725920,42894471,2725923,42894468,2725922,42894469,2725925,2725924,42894467,2725927,2725926,2725945,2725944,2725947,2725946,2725949,2725948,2725951,2725950,2725937,2725936,42894484,2725939,2725938,42894482,2725941,42894483,2725940,42894480,2725943,2725942,42894481,2725897,2725896,2725899,2725898,2725901,2725900,2725903,2725902,2725913,2725912,2725915,2725914,2725917,2725916,2725919,2725918,2725905,2725904,2725907,2725906,2725909,2725908,2725911,2725910,2725481,2725480,2725483,2725482,2725484,2725473,2725472,2725475,2725474,2725477,2725476,2725479,2725478,2725961,2725960,2725963,2725962,44789455,2725965,2725964,2725967,2725966,2725953,2313803,2725952,2313802,2725955,2313801,2725954,2725957,2725956,2725959,2313804,2725958,2725465,42894590,2725464,42894591,42894588,2725467,2725466,42894589,42894586,2725469,42894587,2725468,2725471,42894584,2725470,42894585,2725969,2725968,2725970,43527908,43527909,43533352,43533353,43533247,4031850,43533248,42894606,42894607,42894604,42894605,42894602,42894603,42894600,42894601,42894598,42894599,42894596,42894597,42894594,42894595,42894592,42894593,42894622,42894623,42894620,42894621,42894618,42894619,42894616,42894617,42894614,42894615,42894612,42894613,42894610,42894611,42894608,42894609,43527994,43527995,43527996,43527997,43527998,42894632,42894633,43527999,42894630,42894631,42894628,42894629,42894626,42894627,42894624,42894625,42894654,42894655,42894652,42894653,42894650,42894651,42894648,42894649,42894646,42894647,42894644,42894645,42894643,42894670,42894671,42894668,42894669,42894666,42894667,42894664,42894665,42894662,42894663,42894660,42894661,42894658,42894659,42894656,42894657,42894686,42894687,42894684,42894685,42894682,42894683,42894680,42894681,42894678,42894679,42894676,42894677,42894674,42894675,42894672,42894673,42894702,42894703,42894700,42894701,42894698,42894699,42894696,42894697,42894694,42894695,42894692,42894693,42894690,42894691,42894688,42894689,42894718,42894716,42894717,42894714,42894715,42894712,42894713,42894710,42894711,42894708,42894709,42894706,42894707,42894704,42894705))

) I
) C;

UPDATE STATISTICS #Codesets;


SELECT event_id, person_id, start_date, end_date, op_start_date, op_end_date, visit_occurrence_id
INTO #qualified_events
FROM 
(
  select pe.event_id, pe.person_id, pe.start_date, pe.end_date, pe.op_start_date, pe.op_end_date, row_number() over (partition by pe.person_id order by pe.start_date ASC) as ordinal, cast(pe.visit_occurrence_id as bigint) as visit_occurrence_id
  FROM (-- Begin Primary Events
select P.ordinal as event_id, P.person_id, P.start_date, P.end_date, op_start_date, op_end_date, cast(P.visit_occurrence_id as bigint) as visit_occurrence_id
FROM
(
  select E.person_id, E.start_date, E.end_date,
         row_number() OVER (PARTITION BY E.person_id ORDER BY E.sort_date ASC, E.event_id) ordinal,
         OP.observation_period_start_date as op_start_date, OP.observation_period_end_date as op_end_date, cast(E.visit_occurrence_id as bigint) as visit_occurrence_id
  FROM 
  (
  -- Begin Procedure Occurrence Criteria
select C.person_id, C.procedure_occurrence_id as event_id, C.start_date, C.end_date,
       C.visit_occurrence_id, C.start_date as sort_date
from 
(
  select po.person_id,po.procedure_occurrence_id,po.procedure_concept_id,po.visit_occurrence_id,po.quantity,po.procedure_date as start_date, DATEADD(day,1,po.procedure_date) as end_date 
  FROM @cdm_database_schema.PROCEDURE_OCCURRENCE po
JOIN #Codesets cs on (po.procedure_concept_id = cs.concept_id and cs.codeset_id = 0)
) C


-- End Procedure Occurrence Criteria

UNION ALL
-- Begin Device Exposure Criteria
select C.person_id, C.device_exposure_id as event_id, C.start_date, C.end_date,
        C.visit_occurrence_id, C.start_date as sort_date
from 
(
  select de.person_id,de.device_exposure_id,de.device_concept_id,de.visit_occurrence_id,de.quantity,de.device_exposure_start_date as start_date, COALESCE(de.device_exposure_end_date, DATEADD(day,1,de.device_exposure_start_date)) as end_date 
  FROM @cdm_database_schema.DEVICE_EXPOSURE de
JOIN #Codesets cs on (de.device_concept_id = cs.concept_id and cs.codeset_id = 0)
) C


-- End Device Exposure Criteria

  ) E
	JOIN @cdm_database_schema.observation_period OP on E.person_id = OP.person_id and E.start_date >=  OP.observation_period_start_date and E.start_date <= op.observation_period_end_date
  WHERE DATEADD(day,0,OP.OBSERVATION_PERIOD_START_DATE) <= E.START_DATE AND DATEADD(day,0,E.START_DATE) <= OP.OBSERVATION_PERIOD_END_DATE
) P

-- End Primary Events
) pe
  
) QE

;

--- Inclusion Rule Inserts

create table #inclusion_events (inclusion_rule_id bigint,
	person_id bigint,
	event_id bigint
);

select event_id, person_id, start_date, end_date, op_start_date, op_end_date
into #included_events
FROM (
  SELECT event_id, person_id, start_date, end_date, op_start_date, op_end_date, row_number() over (partition by person_id order by start_date ASC) as ordinal
  from
  (
    select Q.event_id, Q.person_id, Q.start_date, Q.end_date, Q.op_start_date, Q.op_end_date, SUM(coalesce(POWER(cast(2 as bigint), I.inclusion_rule_id), 0)) as inclusion_rule_mask
    from #qualified_events Q
    LEFT JOIN #inclusion_events I on I.person_id = Q.person_id and I.event_id = Q.event_id
    GROUP BY Q.event_id, Q.person_id, Q.start_date, Q.end_date, Q.op_start_date, Q.op_end_date
  ) MG -- matching groups

) Results

;

-- date offset strategy

select event_id, person_id, 
  case when DATEADD(day,1,start_date) > op_end_date then op_end_date else DATEADD(day,1,start_date) end as end_date
INTO #strategy_ends
from #included_events;


-- generate cohort periods into #final_cohort
select person_id, start_date, end_date
INTO #cohort_rows
from ( -- first_ends
	select F.person_id, F.start_date, F.end_date
	FROM (
	  select I.event_id, I.person_id, I.start_date, CE.end_date, row_number() over (partition by I.person_id, I.event_id order by CE.end_date) as ordinal
	  from #included_events I
	  join ( -- cohort_ends
-- cohort exit dates
-- End Date Strategy
SELECT event_id, person_id, end_date from #strategy_ends

    ) CE on I.event_id = CE.event_id and I.person_id = CE.person_id and CE.end_date >= I.start_date
	) F
	WHERE F.ordinal = 1
) FE;


select person_id, min(start_date) as start_date, DATEADD(day,-1 * 0, max(end_date)) as end_date
into #final_cohort
from (
  select person_id, start_date, end_date, sum(is_start) over (partition by person_id order by start_date, is_start desc rows unbounded preceding) group_idx
  from (
    select person_id, start_date, end_date, 
      case when max(end_date) over (partition by person_id order by start_date rows between unbounded preceding and 1 preceding) >= start_date then 0 else 1 end is_start
    from (
      select person_id, start_date, DATEADD(day,0,end_date) as end_date
      from #cohort_rows
    ) CR
  ) ST
) GR
group by person_id, group_idx;

DELETE FROM @target_database_schema.@target_cohort_table where cohort_definition_id = @target_cohort_id;
INSERT INTO @target_database_schema.@target_cohort_table (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
select @target_cohort_id as cohort_definition_id, person_id, start_date, end_date 
FROM #final_cohort CO
;




TRUNCATE TABLE #strategy_ends;
DROP TABLE #strategy_ends;


TRUNCATE TABLE #cohort_rows;
DROP TABLE #cohort_rows;

TRUNCATE TABLE #final_cohort;
DROP TABLE #final_cohort;

TRUNCATE TABLE #inclusion_events;
DROP TABLE #inclusion_events;

TRUNCATE TABLE #qualified_events;
DROP TABLE #qualified_events;

TRUNCATE TABLE #included_events;
DROP TABLE #included_events;

TRUNCATE TABLE #Codesets;
DROP TABLE #Codesets;

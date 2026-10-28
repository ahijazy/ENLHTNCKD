CREATE TABLE #Codesets (
  codeset_id int NOT NULL,
  concept_id bigint NOT NULL
)
;

INSERT INTO #Codesets (codeset_id, concept_id)
SELECT 0 as codeset_id, c.concept_id FROM (select distinct I.concept_id FROM
( 
  select concept_id from @vocabulary_database_schema.CONCEPT where (concept_id in (37167956,4009785,4035316,4098760,437247,4147491,4186108,4218974,37395652,37399441,36716259,37204308,3661615,4311397,765176,4061151,4125628,4121287,4172446,42537687,4231887,4130189,4098008,4048620,439777,4147365,4187768,435789,45773071,37312032,4311673,954484,4140545,4178677,28396,443721,4184200,432452,4281394,4100991,4125491,4146088,4187034,26942,4188474,4243950,4219117,4311676,4130193,4121110,4173028,4305588,441269,30683,45768813,36713571,4218794,35624317,36674519,436659,4039536,37168560,4130192,4035974,4101573,4098027,4125633,4081520,4209085,4308125,1245854,954491,3194085,606940,4131915,4173191,4306199,30978,4185887,954172,35122712,1340246,602960,4098009,4235788,432875,4175331,4206007,3661626,4050010,45757092,4009784,4130194,4090690,4345236,432282,4261354,37397036,4327824,37312165,443726,438722,4338976,4232474,35122714,4122932,4144956,4171201,37397537,4006468,4100985,4098754,4097961,140681,4150547,4177177,4243676,4264909,434156,4320651,35623407,35625536,4219359,1075863,4311538,22281,4097998,4098758,4147911,36715580,36716126,4319914,4187256,45757189,4082738,4098145,4120450,4097214,435515,4146086,4157495,4148471,4214947,37116298,4327045,1244473,35104079,1077005,318397,4008663,4130067,4098740,4102056,4132085,4034963,315523,37109777,44810002,1246711,37165066,434616,4035282,4278920,4174701,4204062,4241982,36716203,432295,4318674,4093515,606065,4038772,437834,4121114,137829,4299560,4247416,4300295,3193947,4282785,4009788,4339722,4159967,443738,4223896,4247425,1246248,4131128,4039785,4120448,4098131,4213820,444289,4234973,42889237,4222774,607430,42872405,4079852,4146771,4135931,4267646,4192278,4287402,36674250,4031827,4098017,4121112,40491316,432588,37017165,42539413,4254380,4302298,4303199,4032006,4101001,4329173,4122926,601114,4195171,45768812,37017132,4250028,760844,4088386,138723,4347537,4181143,4267282,4297024,4196206,4098013,4101584,4159651,4174412,4168911,4239484,37116301,4247579,43562686,4098018,4122930,36713112,4149183,4292531,4198102,36714258,4009306,603011,4130195,23988,4122923,434622,4149681,4155187,4143629,37398911,42596488,4203291,37209558,4100998,4125499,4114026,24909,4290233,4338370,197253,606061,760845,4098753,4120454,40478891,37017285,37017607,37111627,36674808,4336555,4238731,606924,4098627,4120455,4105919,601067,4184758,4246105,45773534,4213893,4218100,4003186,37165049,4176884,600662,25518,4101917,4260689,4242111,4287574,45766392,4193514,4315393,1244936,35104314,4035266,4031662,4101578,4071073,4121113,437833,42536958,36674478,4099889,4144811,603018,4131127,4101458,4201444,4200254,4203904,4265915,4280354,36713763,36714288,37117164,4209139,954488,4079181,46272744,4298975,1246710,4308062,606923,432881,4032352,4098759,4121115,4132431,4122079,601066,24006,4185253,45768941,37397197,36715351,45773070,4314111,4339113,45757147,606928,603007,607422,4146936,4263315,4178731,37109690,37110336,4135713,1244954,4160238,444238,606948,4015896,4018378,4130680,4082917,4121106,4104541,4190190,4187773,37117740,4194519,4312021,42599242,42596091,42596487,606952,4098762,4121107,4196102,37018722,36713970,4217370,37116300,37165630,4144077,4122927,4139556,321263,4181743,36713572,36714965,36717581,37110727,37204478,4022198,45757146,607421,4156842,4174376,4269764,36714533,37119138,4195271,4248254,4149042,4021466,606950,607389,4098028,4100962,4278816,37116297,4269919,1246249,4242755,4008273,4098747,4125493,4143167,4271753,433168,4219853,36715492,1244157,37166856,434610,4101582,4046563,4099603,4160887,40485018,37110923,4253072,954489,441258,434894,4151502,4168441,40599994,4211695,37399453,36715493,36676579,37204551,4220697,42599372,42596486,4209094,4286660,37396761,436956,37204236,4006467,606027,4130191,4044728,4101583,4071444,4145277,435503,4215791,37311954,4228681,954490,37016121,4045142,4121109,4262948,140065,36715494,35624756,37160905,606052,606965,4031699,4100992,4024671,4125494,4171026,4273847,4198185,4221567,1246709,4099005,4042927,4228805,4284386,4238904,35104088,4139775,4131914,4035988,4082253,4100987,4052711,4085853,4147600,4189321,4264017,4287844,36715009,4228194,42596489,4098749,4120451,4096927,4168286,4284415,36713573,4231453,1246707,1340424,4130678,4031204,4158891,4115393,4323223,443961,4280070,42536530,4312853,4215784,37167983,4099224,4290596,4168772,4219253,36674474,4131917,4034711,22288,4243831,602143,4214023,4009786,37016151,4131919,4122924,4143351,4119134,4116343,4307469,4211348,4188208,440979,37019193,4307799,4009304,4019001,4122928,37019055,1246708,43562670,4060269,4297537,434701,4291002,4254249,606921,4098019,4023612,4122931,40481595,4258685,4244129,4268894,35623048,4225810,4330322,4223031,4278669,36716460,1246712,4150499,4131913,4140101,4184603,4297017,4298690,4298813,4313413,4207240,4021911,607426,4130068,4098007,4138560,4228444,43022052,4206913,4271197,37110070,4247776,42536531,440977,608596,4058246,4092893,4313581))

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
  -- Begin Condition Occurrence Criteria
SELECT C.person_id, C.condition_occurrence_id as event_id, C.start_date, C.end_date,
  C.visit_occurrence_id, C.start_date as sort_date
FROM 
(
  SELECT co.person_id,co.condition_occurrence_id,co.condition_concept_id,co.visit_occurrence_id,co.condition_start_date as start_date, COALESCE(co.condition_end_date, DATEADD(day,1,co.condition_start_date)) as end_date 
  FROM @cdm_database_schema.CONDITION_OCCURRENCE co
  JOIN #Codesets cs on (co.condition_concept_id = cs.concept_id and cs.codeset_id = 0)
) C


-- End Condition Occurrence Criteria

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

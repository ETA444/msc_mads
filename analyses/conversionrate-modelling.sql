/* see what customer table looks like */
SELECT * 
FROM customer
ORDER BY customer_id
LIMIT 10;


/* last order date is wrong in the customer table and/or articeevents/order table! PUT THIS IN THE REPORT */
SELECT last_order_date 
FROM customer
WHERE customer_id = '0000BA8847367C96D875E15C880A2562';
SELECT article_event_date, article_event_type
FROM articleevents
WHERE customer_id = '0000BA8847367C96D875E15C880A2562' AND article_event_date > '2022-07-01';
SELECT * 
FROM public.order
WHERE customer_id = '0000BA8847367C96D875E15C880A2562';
/* THIS CUSTOMER DOES NOT EXIST IN THE CUSTOMER TABLE, BUT DOES IN THE SESSION TABLE, ALSO PUT THIS IN REPORT */
SELECT *
FROM customer
WHERE customer_id = 'E790F5B013B583670687E2F6694687F5';
SELECT * 
FROM public.session
WHERE customer_id = 'E790F5B013B583670687E2F6694687F5'
LIMIT 10;


/* the first table we can work from, no age class since all ages are 122 */
CREATE TEMP TABLE tablebase AS 
SELECT customer_id, 
        2022 - start_year AS relationship_length, 
        2022 - birth_year AS age,
        last_order_date, 
        CASE WHEN sex = 'M' THEN 1 WHEN sex = 'U' THEN 1 ELSE 0 END AS male_u,
        CASE WHEN sex = 'F' THEN 1 ELSE 0 END AS female
FROM customer;

SELECT * 
FROM tablebase 
LIMIT 10;

/* check if there are the same number of session in both tables, can safely use article event table*/
SELECT COUNT(DISTINCT session_id)
FROM public.session;
SELECT COUNT(DISTINCT session_id)
FROM articleevents;

/* begin making the conversion column*/
CREATE TEMP TABLE n_sessions_table AS
SELECT customer_id, CAST(COUNT(DISTINCT session_id) AS float) AS n_sessions
FROM articleevents
GROUP BY customer_id;

CREATE TEMP TABLE n_sale_sessions_table AS
SELECT customer_id, CAST(COUNT(DISTINCT session_id) AS float) AS n_sale_sessions
FROM articleevents
WHERE article_event_type = 40
GROUP BY customer_id;

/*not all customers have bought something in sessions*/
SELECT COUNT(DISTINCT customer_id)
FROM n_sessions_table;
SELECT COUNT(DISTINCT customer_id)
FROM n_sale_sessions_table;

/* make conversion and join it with main table*/ 
CREATE TEMP TABLE conversion_table AS
SELECT nst.customer_id, nsst.n_sale_sessions, nst.n_sessions 
FROM n_sessions_table nst
LEFT JOIN n_sale_sessions_table nsst
ON nst.customer_id = nsst.customer_id;

CREATE TEMP TABLE table1 AS
SELECT tb.*, ct.n_sale_sessions/ct.n_sessions AS conversion_rate
FROM tablebase tb 
LEFT JOIN conversion_table ct 
ON tb.customer_id = ct.customer_id;

/* The main table with conversion rate and no NULL */
CREATE TEMP TABLE table2 AS 
SELECT customer_id, age, relationship_length, male_u, female,
        CASE WHEN conversion_rate IS NULL THEN 0 ELSE conversion_rate END AS conversion_rate,
        last_order_date
FROM table1;

SELECT * 
FROM table2
LIMIT 20;

/* count number of people who have bought something from wehkamp since januari*/
SELECT COUNT(*)
FROM table2
WHERE conversion_rate <> 0;

/* begin making the fav channel column */
CREATE TEMP TABLE channel_count_table AS
SELECT customer_id, channel, COUNT(channel) AS n_uses
FROM public.order 
GROUP BY customer_id, channel;
/* select for the most choses channel by inner joining on itself*/
CREATE TEMP TABLE fav_channel_table AS 
SELECT t1.customer_id, MAX(t1.channel) AS fav_channel, MAX(t1.n_uses) AS n_uses
FROM channel_count_table t1
INNER JOIN (SELECT customer_id, MAX(n_uses) AS max_uses
            FROM channel_count_table
            GROUP BY customer_id) t2
ON t1.customer_id = t2.customer_id AND t1.n_uses = t2.max_uses
GROUP BY t1.customer_id;

/* merge with the main table*/
CREATE TEMP TABLE table3 AS
SELECT t1.*, 
        CASE WHEN t2.fav_channel IS NULL THEN 'no_orders' ELSE t2.fav_channel END AS fav_channel
FROM table2 t1
LEFT JOIN fav_channel_table t2
ON t1.customer_id = t2.customer_id;

/*check the table, seems to all be fine, having a fav channel implies a positive conversion rate*/
SELECT * 
FROM table3
WHERE fav_channel <> 'no_orders'
LIMIT 10;

/* easy to make a fav article group since we did that in lecture 3*/
/*get number of purchases from the subcategories*/ 
CREATE TEMP TABLE art_subcat1 AS
SELECT ord.customer_id , sub_category, SUM(ord.items) AS n_purchases
FROM public.order ord
LEFT JOIN article art ON ord.article_id = art.article_id
GROUP BY ord.customer_id, art.sub_category;

SELECT * 
FROM art_subcat1
ORDER BY customer_id
LIMIT 10;

/*get the max n_purchases, using a subquery makes the most sense, keep the rows with the highest purchases, but we get ties! use min*/
CREATE TEMP TABLE maxorder_cat AS
SELECT t1.customer_id, MIN(t1.sub_category) AS sub_category, MIN(t1.n_purchases) AS n_purchases
FROM art_subcat1 t1
INNER JOIN (SELECT customer_id, MAX(n_purchases) AS max_purchases
            FROM art_subcat1
            GROUP BY customer_id) t2
ON t1.customer_id = t2.customer_id AND t1.n_purchases = t2.max_purchases
GROUP BY t1.customer_id;

SELECT *
FROM maxorder_cat 
ORDER BY customer_id
LIMIT 10;

/* merge with the main table to, can make them into dummy variables in R */
CREATE TEMP TABLE table4 AS 
SELECT t1.*, CASE WHEN t2.sub_category IS NULL THEN 'no_purchases' ELSE t2.sub_category END AS fav_art_group 
FROM table3 t1
LEFT JOIN maxorder_cat t2
ON t1.customer_id = t2.customer_id;
/*looks fine*/
SELECT * 
FROM table4
WHERE fav_art_group <> 'no_purchases'
LIMIT 10;

/* start work on the acceptable price range column, can get everything from orders, since articles are ordered 1 by 1*/
/*sales amount is current price of the article that is ordered, because of the way order table is structered*/
CREATE TEMP TABLE price_range_table AS 
SELECT customer_id, MIN(sales_amount) AS min_price, MAX(sales_amount) AS max_price
FROM public.order 
GROUP BY customer_id;

SELECT *
FROM price_range_table
LIMIT 10;

/* merge it with the main table, can make ranges in R, probably easier, Letting them be NULLs for 0 purchases makes sense*/
CREATE TEMP TABLE table5 AS
SELECT t1.*, t2.min_price AS min_price_art, t2.max_price AS max_price_art
FROM table4 t1
LEFT JOIN  price_range_table t2
ON t1.customer_id = t2.customer_id;
/*looks fine*/
SELECT *
FROM table5
WHERE min_price_art IS NOT NULL
LIMIT 10;

/* change the main table to a session focused table, 168893 distinct sessions */
CREATE TEMP TABlE session_table1 AS
SELECT t1.session_id, t1.customer_id, t1.session_date, age AS cust_age , relationship_length AS cust_relationship_length, male_u, female,
        conversion_rate AS cust_conversion_rate, last_order_date AS cust_last_order_date, fav_channel AS cust_fav_channel,
        fav_art_group AS cust_fav_art_group, min_price_art AS cust_min_price, max_price_art AS cust_max_price
FROM public.session t1
LEFT JOIN table5 t2
ON t1.customer_id = t2.customer_id;

SELECT *
FROM session_table1
LIMIT 10;

/* making a conversion rate per session, using type10's and type 40's to calculate the conversion rate, here we get the base info we need*/
CREATE TEMP TABLE session_sumtypes_table AS
SELECT session_id,
        CAST(SUM(CASE WHEN article_event_type = '10' THEN 1 ELSE 0 END) AS float) AS type_10,
        SUM(CASE WHEN article_event_type = '20' THEN 1 ELSE 0 END) AS type_20,
        SUM(CASE WHEN article_event_type = '30' THEN 1 ELSE 0 END) AS type_30,
        CAST(SUM(CASE WHEN article_event_type = '40' THEN 1 ELSE 0 END) AS float) AS type_40,
        SUM(CASE WHEN article_event_type = '50' THEN 1 ELSE 0 END) AS type_50
FROM articleevents
GROUP BY session_id;

/* calculating the actual session conversion rate, some are gonna be over 100 percent, fix that in the next table */
CREATE TEMP TABLE session_conversion_rate_table1 AS
SELECT session_id, 
        100.0 * (CASE WHEN type_10 > 0 AND type_40 > 0 THEN type_40/type_10
        WHEN type_10 = 0 AND type_40 > 0 THEN 1.0
        WHEN type_10 > 0 AND type_40 = 0 THEN 0.0 
        WHEN type_10 = 0 AND type_40 = 0 THEN 0.0 END) AS conversion_rate_session
FROM session_sumtypes_table;

/* sanity check: in total 658 have conversion rate above 100%, might affect our result, need to fix it */

SELECT MIN(conversion_rate_session), MAX(conversion_rate_session)
FROM session_conversion_rate_table1;

SELECT *
FROM session_conversion_rate_table1
WHERE conversion_rate_session = 1800.0;

SELECT *
FROM session_sumtypes_table
WHERE session_id = 'C59521C4E70438327E154DB72F7258D9' OR session_id = '3D5DA9F9A656255C9CB9982D78B9DB95' OR session_id = '04B1C313D259FCD316A943246E177519';

SELECT COUNT(*)
FROM session_conversion_rate_table1
WHERE conversion_rate_session > 100.0;

/* sanity check ends */

CREATE TEMP TABLE session_conversion_rate_table2 AS
SELECT session_id,
        CAST((CASE WHEN conversion_rate_session > 100 THEN 100.00 ELSE conversion_rate_session END) AS DECIMAL(5,2)) AS conversion_rate_session
FROM session_conversion_rate_table1;

SELECT *
FROM session_conversion_rate_table2
ORDER BY conversion_rate_session DESC
LIMIT 100;

/*merging it with the main table to be used */
CREATE TEMP TABlE session_table2 AS
SELECT t1.*, t2.conversion_rate_session AS session_conversion_rate
FROM session_table1 t1
LEFT JOIN session_conversion_rate_table2 t2
ON t1.session_id = t2.session_id;

SELECT * 
FROM session_table2
LIMIT 10;

/* make a dummy variables that tells us if the session has had a sale or not*/
CREATE TEMP TABLE session_sale_dummy_table AS
SELECT session_id, 
        CASE WHEN SUM(CASE WHEN article_event_type = '40' THEN 1 ELSE 0 END) > 0 THEN 1 ELSE 0 END AS session_sale_dummy,
        SUM(CASE WHEN article_event_type = '10' THEN 1 ELSE 0 END) AS n_session_views,
        SUM(CASE WHEN article_event_type = '40' THEN 1 ELSE 0 END) AS n_session_sales
FROM articleevents
GROUP BY session_id;

/* add it to the main table to be used*/
CREATE TEMP TABlE session_table3 AS
SELECT t1.*, t2.session_sale_dummy, t2.n_session_views, t2.n_session_sales
FROM session_table2 t1
LEFT JOIN session_sale_dummy_table t2
ON t1.session_id = t2.session_id;

SELECT *
FROM session_table3
LIMIT 10;
 
/* check if we still have all the sessions, we do !*/
SELECT COUNT(DISTINCT session_id)
FROM session_table3;

/* NO CUSTOMER LOYALTY, WEEK-WEEKEND SESSION, RAINY/CLOUDY/SUNNY SESSION OR COIVD SESSION DATA IN TABLE YET, ALSO FIX CUST CONVERSION RATE*/
/* customer loyalty variable make */
CREATE TEMP TABLE jan_table AS
SELECT customer_id, COUNT(DISTINCT session_id) AS n_jan_custsessions, SUM(CASE WHEN article_event_type = '40' THEN 1 ELSE 0 END) AS n_jan_sale
FROM public.articleevents
WHERE article_event_date BETWEEN '2022-01-01' AND '2022-01-31'
GROUP BY customer_id;

CREATE TEMP TABLE feb_table AS
SELECT customer_id, COUNT(DISTINCT session_id) AS n_feb_custsessions, SUM(CASE WHEN article_event_type = '40' THEN 1 ELSE 0 END) AS n_feb_sale
FROM public.articleevents
WHERE article_event_date BETWEEN '2022-02-01' AND '2022-02-28'
GROUP BY customer_id;

CREATE TEMP TABLE mar_table AS
SELECT customer_id, COUNT(DISTINCT session_id) AS n_mar_custsessions, SUM(CASE WHEN article_event_type = '40' THEN 1 ELSE 0 END) AS n_mar_sale
FROM public.articleevents
WHERE article_event_date BETWEEN '2022-03-01' AND '2022-03-31'
GROUP BY customer_id;

CREATE TEMP TABLE apr_table AS
SELECT customer_id, COUNT(DISTINCT session_id) AS n_apr_custsessions, SUM(CASE WHEN article_event_type = '40' THEN 1 ELSE 0 END) AS n_apr_sale
FROM public.articleevents
WHERE article_event_date BETWEEN '2022-04-01' AND '2022-04-30'
GROUP BY customer_id;

CREATE TEMP TABLE may_table AS
SELECT customer_id, COUNT(DISTINCT session_id) AS n_may_custsessions, SUM(CASE WHEN article_event_type = '40' THEN 1 ELSE 0 END) AS n_may_sale
FROM public.articleevents
WHERE article_event_date BETWEEN '2022-05-01' AND '2022-05-31'
GROUP BY customer_id;

CREATE TEMP TABLE jun_table AS
SELECT customer_id, COUNT(DISTINCT session_id) AS n_jun_custsessions, SUM(CASE WHEN article_event_type = '40' THEN 1 ELSE 0 END) AS n_jun_sale
FROM public.articleevents
WHERE article_event_date BETWEEN '2022-06-01' AND '2022-06-30'
GROUP BY customer_id;

CREATE TEMP TABLE jul_table AS
SELECT customer_id, COUNT(DISTINCT session_id) AS n_jul_custsessions, SUM(CASE WHEN article_event_type = '40' THEN 1 ELSE 0 END) AS n_jul_sale
FROM public.articleevents
WHERE article_event_date BETWEEN '2022-07-01' AND '2022-07-31'
GROUP BY customer_id;

CREATE TEMP TABLE monthly_table AS
SELECT  t0.customer_id,
        t1.n_jan_custsessions, t1.n_jan_sale,
        t2.n_feb_custsessions, t2.n_feb_sale,
        t3.n_mar_custsessions, t3.n_mar_sale,
        t4.n_apr_custsessions, t4.n_apr_sale,
        t5.n_may_custsessions, t5.n_may_sale,
        t6.n_jun_custsessions, t6.n_jun_sale,
        t7.n_jul_custsessions, t7.n_jul_sale
FROM customer t0
LEFT JOIN jan_table t1
ON t0.customer_id = t1.customer_id
LEFT JOIN feb_table t2
ON t0.customer_id = t2.customer_id 
LEFT JOIN mar_table t3
ON t0.customer_id = t3.customer_id
LEFT JOIN apr_table t4
ON t0.customer_id = t4.customer_id 
LEFT JOIN may_table t5
ON t0.customer_id = t5.customer_id 
LEFT JOIN jun_table t6
ON t0.customer_id = t6.customer_id 
LEFT JOIN jul_table t7
ON t0.customer_id = t7.customer_id;

SELECT * 
FROM monthly_table
LIMIT 10;

CREATE TEMP TABLE loyal_table AS
SELECT customer_id, 
        CASE WHEN n_may_sale > 0 AND n_jun_sale > 0 AND n_jul_sale > 0 THEN 1 ELSE 0 END AS loyal_sale_dummy,
        CASE WHEN n_may_custsessions > 0 AND n_jun_custsessions > 0 AND n_jul_custsessions > 0 THEN 1 ELSE 0 END AS loyal_session_dummy
FROM monthly_table
ORDER BY loyal_session_dummy DESC;


CREATE TEMP TABLE session_table4 AS
SELECT t1.*, t2.loyal_sale_dummy AS loyal_sale_cust, t2.loyal_session_dummy AS loyal_session_cust
FROM session_table3 t1
LEFT JOIN loyal_table t2
On t1.customer_id = t2.customer_id;

SELECT *
FROM session_table4
LIMIT 10;

SELECT *
FROM session_table4;

SELECT COUNT(DISTINCT customer_id)
FROM session_table4;

/* SANITY CHECKS */

/* same session number: 168893 */
SELECT COUNT(DISTINCT session_id)
FROM session_table4;

/* GOOD! we get a result of 5.36, adjusted compared to 5.75 (result of Disaster check – conversion-% (sales/views)) */
SELECT AVG(session_conversion_rate) AS avg_cr_session
FROM session_table4;

SELECT MIN(session_date), MAX(session_date)
FROM session_table4;

SELECT MIN(cust_relationship_length), MAX(cust_relationship_length), AVG(cust_relationship_length)
FROM session_table4;

/* total number of loyal session: 2518 */
SELECT COUNT(DISTINCT customer_id)
FROM session_table4
WHERE loyal_session_cust = 1;

/* total number of loyal sale: 87 */
SELECT COUNT(DISTINCT customer_id)
FROM session_table4
WHERE loyal_sale_cust = 1;

/* One missing value in both loyal_sale and loyal_session:
"E79..." THIS CUSTOMER DOES NOT EXIST */
SELECT *
FROM session_table4
WHERE loyal_sale_cust IS NULL OR loyal_session_cust IS NULL;

/* SANITY CHECKS ENDS */
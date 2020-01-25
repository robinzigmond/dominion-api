--
-- PostgreSQL database dump
--

-- Dumped from database version 9.6.5
-- Dumped by pg_dump version 9.6.5

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: card; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE card (
    id bigint NOT NULL,
    name character varying NOT NULL,
    set character varying NOT NULL,
    coin_cost bigint,
    potion_cost boolean,
    debt_cost bigint,
    main_text character varying NOT NULL,
    other_text character varying,
    is_kingdom boolean NOT NULL,
    non_terminal character varying,
    gives_extra_actions character varying,
    returns_card character varying,
    increases_hand_size character varying,
    extra_buy character varying,
    trashes boolean NOT NULL
);


ALTER TABLE card OWNER TO postgres;

--
-- Name: card_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE card_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE card_id_seq OWNER TO postgres;

--
-- Name: card_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE card_id_seq OWNED BY card.id;


--
-- Name: link_pairs; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE link_pairs (
    id bigint NOT NULL,
    card_one bigint NOT NULL,
    card_two bigint NOT NULL
);


ALTER TABLE link_pairs OWNER TO postgres;

--
-- Name: link_pairs_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE link_pairs_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE link_pairs_id_seq OWNER TO postgres;

--
-- Name: link_pairs_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE link_pairs_id_seq OWNED BY link_pairs.id;


--
-- Name: type; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE type (
    id bigint NOT NULL,
    name character varying NOT NULL
);


ALTER TABLE type OWNER TO postgres;

--
-- Name: type_card; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE type_card (
    id bigint NOT NULL,
    card_id bigint NOT NULL,
    type_id bigint NOT NULL
);


ALTER TABLE type_card OWNER TO postgres;

--
-- Name: type_card_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE type_card_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE type_card_id_seq OWNER TO postgres;

--
-- Name: type_card_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE type_card_id_seq OWNED BY type_card.id;


--
-- Name: type_id_seq; Type: SEQUENCE; Schema: public; Owner: postgres
--

CREATE SEQUENCE type_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE type_id_seq OWNER TO postgres;

--
-- Name: type_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: postgres
--

ALTER SEQUENCE type_id_seq OWNED BY type.id;


--
-- Name: card id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY card ALTER COLUMN id SET DEFAULT nextval('card_id_seq'::regclass);


--
-- Name: link_pairs id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY link_pairs ALTER COLUMN id SET DEFAULT nextval('link_pairs_id_seq'::regclass);


--
-- Name: type id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY type ALTER COLUMN id SET DEFAULT nextval('type_id_seq'::regclass);


--
-- Name: type_card id; Type: DEFAULT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY type_card ALTER COLUMN id SET DEFAULT nextval('type_card_id_seq'::regclass);


--
-- Data for Name: card; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY card (id, name, set, coin_cost, potion_cost, debt_cost, main_text, other_text, is_kingdom, non_terminal, gives_extra_actions, returns_card, increases_hand_size, extra_buy, trashes) FROM stdin;
1	copper	Base	0	f	0	1 Coin	\N	f	\N	\N	\N	\N	\N	f
2	curse	Base	0	f	0	-1 VP	\N	f	\N	\N	\N	\N	\N	f
3	estate	Base	2	f	0	1 VP	\N	f	\N	\N	\N	\N	\N	f
4	silver	Base	3	f	0	2 Coins	\N	f	\N	\N	\N	\N	\N	f
5	duchy	Base	5	f	0	3 VP	\N	f	\N	\N	\N	\N	\N	f
6	gold	Base	6	f	0	3 Coins	\N	f	\N	\N	\N	\N	\N	f
7	province	Base	8	f	0	6 VP	\N	f	\N	\N	\N	\N	\N	f
8	cellar	Base	2	f	0	+1 Action. Discard any number of cards, then draw that many.	\N	t	Always	Never	Never	Never	Never	f
9	chapel	Base	2	f	0	Trash up to 4 cards from your hand.	\N	t	Never	Never	Never	Never	Never	t
10	moat	Base	2	f	0	+2 Cards.	When another player plays an Attack card, you may first reveal this from your hand, to be unaffected by it.	t	Never	Never	Always	Always	Never	f
11	harbinger	BaseSecondEd	3	f	0	+1 Card. +1 Action. Look through your discard pile. You may put a card from it onto your deck.	\N	t	Always	Never	Always	Never	Never	f
12	merchant	BaseSecondEd	3	f	0	+1 Card. +1 Action. The first time you play a Silver this turn, +1 Coin.	\N	t	Always	Never	Always	Never	Never	f
13	vassal	BaseSecondEd	3	f	0	+2 Coins. Discard the top card of your deck. If it's an Action card, you may play it.	\N	t	Sometimes	Sometimes	Sometimes	Sometimes	Sometimes	f
14	village	Base	3	f	0	+1 Card. +2 Actions.	\N	t	Always	Always	Always	Never	Never	f
15	workshop	Base	3	f	0	Gain a card costing up to 4 Coins.	\N	t	Never	Never	Never	Never	Never	f
16	bureaucrat	Base	4	f	0	Gain a Silver onto your deck. Each other player reveals a Victory card from their hand and puts it onto their deck (or reveals a hand with no Victory cards).	\N	t	Never	Never	Never	Never	Never	f
17	gardens	Base	4	f	0	Worth 1VP per 10 cards you have (round down).	\N	t	\N	\N	\N	\N	\N	f
18	militia	Base	4	f	0	+2 Coins. Each other player discards down to 3 cards in hand.	\N	t	Never	Never	Never	Never	Never	f
19	moneylender	Base	4	f	0	You may trash a Copper from your hand for +3 Coins.	\N	t	Never	Never	Never	Never	Never	t
20	poacher	BaseSecondEd	4	f	0	+1 Card. +1 Action. +1 Coin. Discard a card per empty Supply pile.	\N	t	Always	Never	Sometimes	Never	Never	f
22	smithy	Base	4	f	0	+3 Cards	\N	t	Never	Never	Always	Always	Never	f
23	throne-room	Base	4	f	0	You may play an Action card from your hand twice.	\N	t	Sometimes	Sometimes	Sometimes	Sometimes	Sometimes	f
24	bandit	BaseSecondEd	5	f	0	Gain a Gold. Each other player reveals the top 2 cards of their deck, trashes a revealed Treasure other than Copper, and discards the rest.	\N	t	Never	Never	Never	Never	Never	f
26	festival	Base	5	f	0	+2 Actions. +1 Buy. +2 Coins.	\N	t	Always	Always	Never	Never	Always	f
25	council-room	Base	5	f	0	+4 Cards. +1 Buy. Each other player draws a card.	\N	t	Never	Never	Always	Always	Always	f
27	laboratory	Base	5	f	0	+2 Cards. +1 Action.	\N	t	Always	Never	Always	Always	Never	f
28	library	Base	5	f	0	Draw until you have 7 cards in hand, skipping any Action cards you choose to; set those aside, discarding them afterwards.	\N	t	Never	Never	Sometimes	Sometimes	Never	f
29	market	Base	5	f	0	+1 Card. +1 Action. +1 Buy. +1 Coin.	\N	t	Always	Never	Always	Never	Always	f
30	mine	Base	5	f	0	You may trash a Treasure from your hand. Gain a Treasure to your hand costing up to 3 Coins more than it.	\N	t	Never	Never	Never	Never	Never	t
21	remodel	Base	4	f	0	Trash a card from your hand. Gain a card costing up to 2 Coins more than it.	\N	t	Never	Never	Never	Never	Never	t
31	sentry	BaseSecondEd	5	f	0	+1 Card. +1 Action. Look at the top 2 cards of your deck. Trash and/or discard any number of them. Put the rest back on top in any order.	\N	t	Always	Never	Always	Never	Never	t
32	witch	Base	5	f	0	+2 Cards. Each other player gains a Curse.	\N	t	Never	Never	Always	Always	Never	f
33	artisan	BaseSecondEd	6	f	0	Gain a card to your hand costing up to 5 Coins. Put a card from your hand onto your deck.	\N	t	Never	Never	Never	Never	Never	f
34	chancellor	BaseFirstEd	3	f	0	+2 Coins. You may immediately put your deck into your discard pile.	\N	t	Never	Never	Never	Never	Never	f
35	woodcutter	BaseFirstEd	3	f	0	+1 Buy. +2 Coins.	\N	t	Never	Never	Never	Never	Always	f
36	feast	BaseFirstEd	4	f	0	Trash this card. Gain a card costing up to 5 Coins.	\N	t	Never	Never	Never	Never	Never	f
37	spy	BaseFirstEd	4	f	0	+1 Card. +1 Action. Each player (including you) reveals the top card of his deck and either discards it or puts it back, your choice.	\N	t	Always	Never	Always	Never	Never	f
38	thief	BaseFirstEd	4	f	0	Each other player reveals the top 2 cards of his deck. If they revealed any Treasure cards, they trash one of them that you choose. You may gain any or all of these trashed cards. They discard the other revealed cards.	\N	t	Never	Never	Never	Never	Never	f
39	adventurer	BaseFirstEd	6	f	0	Reveal cards from your deck until you reveal 2 Treasure cards. Put those Treasure cards into your hand and discard the other revealed cards.	\N	t	Never	Never	Always	Always	Never	f
40	courtyard	Intrigue	2	f	0	+3 Cards. Put a card from your hand onto your deck.	\N	t	Never	Never	Always	Always	Never	f
41	lurker	IntrigueSecondEd	2	f	0	+1 Action. Choose one: Trash an Action card from the Supply; or gain an Action card from the trash.	\N	t	Always	Never	Never	Never	Never	f
42	pawn	Intrigue	2	f	0	Choose two: +1 Card; +1 Action; +1 Buy; +1 Coin. The choices must be different.	\N	t	Sometimes	Never	Sometimes	Never	Sometimes	f
43	masquerade	Intrigue	3	f	0	+2 Cards. Each player with any cards in hand passes one to the next such player to their left, at once. Then you may trash a card from your hand.	\N	t	Never	Never	Always	Sometimes	Never	t
44	shanty-town	Intrigue	3	f	0	+2 Actions. Reveal your hand. If you have no Action cards in hand, +2 Cards.	\N	t	Always	Always	Sometimes	Sometimes	Never	f
45	steward	Intrigue	3	f	0	Choose one: +2 Cards; or +2 Coins; or trash 2 cards from your hand.	\N	t	Never	Never	Sometimes	Sometimes	Never	t
46	swindler	Intrigue	3	f	0	+2 Coins. Each other player trashes the top card of their deck and gains a card with the same cost that you choose.	\N	t	Never	Never	Never	Never	Never	f
47	wishing-well	Intrigue	3	f	0	+1 Card. +1 Action. Name a card, then reveal the top card of your deck. If you named it, put it into your hand.	\N	t	Always	Never	Always	Sometimes	Never	f
48	baron	Intrigue	4	f	0	+1 Buy. You may discard an Estate for +4 Coins. If you don't, gain an Estate.	\N	t	Never	Never	Never	Never	Always	f
49	bridge	Intrigue	4	f	0	+1 Buy. +1 Coin. This turn, cards (everywhere) cost 1 Coin less, but not less than 0 Coins.	\N	t	Never	Never	Never	Never	Always	f
50	conspirator	Intrigue	4	f	0	+2 Coins. If you've played 3 or more Actions this turn (counting this), +1 Card and +1 Action.	\N	t	Sometimes	Never	Sometimes	Never	Never	f
51	diplomat	IntrigueSecondEd	4	f	0	+2 Cards. If you have 5 or fewer cards in hand (after drawing), +2 Actions.	When another player plays an Attack card, you may first reveal this from a hand of 5 or more cards, to draw 2 cards then discard 3.	t	Sometimes	Sometimes	Always	Always	Never	f
52	ironworks	Intrigue	4	f	0	Gain a card costing up to 4 Coins. If the gained card is an... Action card, +1 Action. Treasure card, +1 Coin. Victory card, +1 Card.	\N	t	Sometimes	Never	Sometimes	Never	Never	f
53	mill	IntrigueSecondEd	4	f	0	+1 Card. +1 Action. You may discard 2 cards, for +2 Coins.	1 VP	t	Always	Never	Sometimes	Never	Never	f
54	mining-village	Intrigue	4	f	0	+1 Card. +2 Actions. You may trash this for +2 Coins.	\N	t	Always	Always	Always	Never	Never	f
55	secret-passage	IntrigueSecondEd	4	f	0	+2 Cards. +1 Action. Take a card from your hand and put it anywhere in your deck.	\N	t	Always	Never	Always	Never	Never	f
56	courtier	IntrigueSecondEd	5	f	0	Reveal a card from your hand. For each type it has (Action, Attack, etc.), choose one: +1 Action; or +1 Buy; or +3 Coins; or gain a Gold. The choices must be different.	\N	t	Sometimes	Never	Never	Never	Sometimes	f
57	duke	Intrigue	5	f	0	Worth 1 VP per Duchy you have.	\N	t	\N	\N	\N	\N	\N	f
58	minion	Intrigue	5	f	0	+1 Action. Choose one: +2 Coins; or discard your hand, +4 Cards, and each other player with at least 5 cards in hand discards their hand and draws 4 cards.	\N	t	Always	Never	Sometimes	Sometimes	Never	f
59	patrol	IntrigueSecondEd	5	f	0	+3 Cards. Reveal the top 4 cards of your deck. Put the Victory cards and Curses into your hand. Put the rest back in any order.	\N	t	Never	Never	Always	Always	Never	f
60	replace	IntrigueSecondEd	5	f	0	Trash a card from your hand. Gain a card costing up to 2 Coins more than it. If the gained card is an Action or Treasure, put it onto your deck; if it's a Victory card, each other player gains a Curse.	\N	t	Never	Never	Never	Never	Never	t
61	torturer	Intrigue	5	f	0	+3 Cards. Each other player either discards 2 cards or gains a Curse to their hand, their choice. (They may pick an option they can't do.)	\N	t	Never	Never	Always	Always	Never	f
62	trading-post	Intrigue	5	f	0	Trash 2 cards from your hand. If you did, gain a Silver to your hand.	\N	t	Never	Never	Never	Never	Never	t
63	upgrade	Intrigue	5	f	0	+1 Card. +1 Action. Trash a card from your hand. Gain a card costing exactly 1 Coin more than it.	\N	t	Always	Never	Never	Never	Never	t
64	harem	Intrigue	6	f	0	2 Coins	2 VP	t	\N	\N	\N	\N	\N	f
65	nobles	Intrigue	6	f	0	Choose one: +3 Cards; or +2 Actions.	2 VP	t	Sometimes	Sometimes	Sometimes	Sometimes	Never	f
66	secret-chamber	IntrigueFirstEd	2	f	0	Discard any number of cards. +1 Coin per card discarded.	When another player plays an Attack card, you may reveal this from your hand. If you do,+2 Cards, then put 2 cards from your hand on top of your deck.	t	Never	Never	Never	Never	Never	f
67	great-hall	IntrigueFirstEd	3	f	0	+1 Card. +1 Action.	1 VP	t	Always	Never	Always	Never	Never	f
68	coppersmith	IntrigueFirstEd	4	f	0	Copper produces an extra 1 Coin this turn.	\N	t	Never	Never	Never	Never	Never	f
69	scout	IntrigueFirstEd	4	f	0	+1 Action. Reveal the top 4 cards of your deck. Put the revealed Victory cards into your hand. Put the other cards on top of your deck in any order.	\N	t	Always	Never	Sometimes	Sometimes	Never	f
71	tribute	IntrigueFirstEd	5	f	0	The player to your left reveals then discards the top 2 cards of his deck. For each differently named card revealed, if it is an... Action Card, +2 Actions. Treasure Card, +2 Coins. Victory Card, +2 Cards.	\N	t	Sometimes	Sometimes	Sometimes	Sometimes	Never	f
70	saboteur	IntrigueFirstEd	5	f	0	Each other player reveals cards from the top of his deck until revealing one costing 3 Coins or more. He trashes that card and may gain a card costing at most 2 Coins less than it. He discards the other revealed cards.	\N	t	Never	Never	Never	Never	Never	f
72	embargo	Seaside	2	f	0	+2 Coins. Trash this. Add an Embargo token to a Supply pile. (For the rest of the game, when a player buys a card from that pile, they gain a Curse.)	\N	t	Never	Never	Never	Never	Never	f
73	haven	Seaside	2	f	0	+1 Card. +1 Action. Set aside a card from your hand face down (under this). At the start of your next turn, put it into your hand.	\N	t	Always	Never	Never	Never	Never	f
74	lighthouse	Seaside	2	f	0	+1 Action. Now and at the start of your next turn: +1 Coin.	While this is in play, when another player plays an Attack card, it doesn't affect you.	t	Always	Never	Never	Never	Never	f
75	native-village	Seaside	2	f	0	+2 Actions. Choose one: Put the top card of your deck face down on your Native Village mat (you may look at those cards at any time); or put all the cards from your mat into your hand.	\N	t	Always	Always	Sometimes	Sometimes	Never	f
104	university	Alchemy	2	t	0	+2 Actions. You may gain an Action card costing up to 5 Coins.	\N	t	Always	Always	Never	Never	Never	f
77	pearl-diver	Seaside	2	f	0	+1 Card. +1 Action. Look at the bottom card of your deck. You may put it on top.	\N	t	Always	Never	Always	Never	Never	f
78	ambassador	Seaside	3	f	0	Reveal a card from your hand. Return up to 2 copies of it from your hand to the Supply. Then each other player gains a copy of it.	\N	t	Never	Never	Never	Never	Never	t
79	fishing-village	Seaside	3	f	0	+2 Actions. +1 Coin. At the start of your next turn: +1 Action and +1 Coin.	\N	t	Always	Always	Never	Never	Never	f
80	lookout	Seaside	3	f	0	+1 Action. Look at the top 3 cards of your deck. Trash one of them. Discard one of them. Put the other one back on top of your deck.	\N	t	Always	Never	Never	Never	Never	t
81	smugglers	Seaside	3	f	0	Gain a copy of a card costing up to 6 Coins that the player to your right gained on their last turn.	\N	t	Never	Never	Never	Never	Never	f
82	warehouse	Seaside	3	f	0	+3 Cards. +1 Action. Discard 3 cards.	\N	t	Always	Never	Never	Never	Never	f
84	cutpurse	Seaside	4	f	0	+2 Coins. Each other player discards a Copper (or reveals a hand with no Copper).	\N	t	Never	Never	Never	Never	Never	f
85	island	Seaside	4	f	0	Put this and a card from your hand onto your Island mat.	2 VP	t	Never	Never	Never	Never	Never	t
86	navigator	Seaside	4	f	0	+2 Coins. Look at the top 5 cards of your deck. Either discard them all, or put them back in any order.	\N	t	Never	Never	Never	Never	Never	f
87	pirate-ship	Seaside	4	f	0	Choose one: +1 Coin per Coin token on your Pirate ship mat; or each other player reveals the top 2 cards of their deck, trashes one of those Treasures that you choose, and discards the rest, and then if anyone trashed a Treasure you add a Coin token to your Pirate Ship mat.	\N	t	Never	Never	Never	Never	Never	f
88	salvager	Seaside	4	f	0	+1 Buy. Trash a card from your hand. +1 Coin per 1 Coin it costs.	\N	t	Never	Never	Never	Never	Always	t
89	sea-hag	Seaside	4	f	0	Each other player discards the top card of their deck, then gains a Curse onto their deck.	\N	t	Never	Never	Never	Never	Never	f
90	treasure-map	Seaside	4	f	0	Trash this and a Treasure Map from your hand. If you trashed two Treasure Maps, gain 4 Golds onto your deck.	\N	t	Never	Never	Never	Never	Never	f
91	bazaar	Seaside	5	f	0	+1 Card. +2 Actions. +1 Coin.	\N	t	Always	Always	Always	Never	Never	f
92	explorer	Seaside	5	f	0	You may reveal a Province from your hand. If you do, gain a Gold to your hand. If you don't gain a Silver to your hand.	\N	t	Never	Never	Always	Never	Never	f
93	ghost-ship	Seaside	5	f	0	+2 Cards. Each other player with 4 or more cards in hand puts cards from their hand onto their deck until they have 3 cards in hand.	\N	t	Never	Never	Always	Always	Never	f
94	merchant-ship	Seaside	5	f	0	Now and at the start of your next turn: +2 Coins.	\N	t	Never	Never	Never	Never	Never	f
95	outpost	Seaside	5	f	0	If this is the first time you played an Outpost this turn, and the previous turn wasn't yours, then take an extra turn after this one, and you only draw 3 cards for your next hand.	\N	t	Never	Never	Never	Never	Never	f
96	tactician	Seaside	5	f	0	If you have at least one card in hand, discard your hand, and at the start of your next turn, +5 Cards, +1 Action, and +1 Buy.	\N	t	Never	Never	Never	Never	Never	f
97	treasury	Seaside	5	f	0	+1 Card. +1 Action. +1 Coin.	When you discard this from play, if you didn't buy a Victory card this turn, you may put this onto your deck.	t	Always	Never	Always	Never	Never	f
98	wharf	Seaside	5	f	0	Now and at the start of your next turn: +2 Cards and +1 Buy.	\N	t	Never	Never	Always	Always	Always	f
83	caravan	Seaside	4	f	0	+1 Card. +1 Action. At the start of your next turn, +1 Card.	\N	t	Always	Never	Always	Never	Never	f
99	transmute	Alchemy	0	t	0	Trash a card from your hand. If it is an... Action card, gain a Duchy. Treasure card, gain a Transmute. Victory card, gain a Gold.	\N	t	Never	Never	Never	Never	Never	t
100	vineyard	Alchemy	0	t	0	Worth 1 VP per 3 Action cards you have (round down).	\N	t	\N	\N	\N	\N	\N	f
101	herbalist	Alchemy	2	f	0	+1 Buy. +1 Coin.	When you discard this from play, you may put one of your Treasures from play onto your deck.	t	Never	Never	Never	Never	Always	f
102	apothecary	Alchemy	2	t	0	+1 Card. +1 Action. Reveal the top 4 cards of your deck. Put the Coppers and Potions into your hand. Put the rest back in any order.	\N	t	Always	Never	Always	Sometimes	Never	f
103	scrying-pool	Alchemy	2	t	0	+1 Action. Each player (including you) reveals the top card of their deck and either discards it or puts it back, your choice. Then reveal cards from your deck until revealing one that isn't an Action. Put all of those revealed cards into your hand.	\N	t	Always	Never	Always	Sometimes	Never	f
105	alchemist	Alchemy	3	t	0	+2 Cards. +1 Action.	When you discard this from play, if you have a Potion in play, you may put this onto your deck.	t	Always	Never	Always	Always	Never	f
106	familiar	Alchemy	3	t	0	+1 Card. +1 Action. Each other player gains a Curse.	\N	t	Always	Never	Always	Never	Never	f
107	philosopher's-stone	Alchemy	3	t	0	When you play this, count your deck and discard pile. Worth 1 Coin per 5 cards total between them (round down).	\N	t	\N	\N	\N	\N	\N	f
108	potion	Alchemy	4	f	0	Potion	\N	f	\N	\N	\N	\N	\N	f
109	golem	Alchemy	4	t	0	Reveal cards from your deck until you reveal 2 Action cards other than Golems. Discard the other cards, then play the Action cards in either order.	\N	t	Sometimes	Sometimes	Sometimes	Sometimes	Sometimes	f
110	apprentice	Alchemy	5	f	0	+1 Action. Trash a card from your hand. +1 Card per 1 Coin it costs. +2 Cards if it has Potion in its cost.	\N	t	Always	Never	Sometimes	Sometimes	Never	t
146	tournament	Cornucopia	4	f	0	+1 Action. Each player may reveal a Province from their hand. If you do, discard it and gain any Prize (from the Prize pile) or a Duchy, onto your deck. If no-one else does, +1 Card and +1 Coin.	\N	t	Always	Never	Sometimes	Never	Never	f
111	possession	Alchemy	6	t	0	The player to your left takes an extra turn after this one, in which you can see all cards they can and make all decisions for them. Any cards or Debt they would gain on that turn, you gain instead; any cards of theirs that are trashed are set aside and put in their discard pile at end of turn.	\N	t	Never	Never	Never	Never	Never	f
112	loan	Prosperity	3	f	0	1 Coin. When you play this, reveal cards from your deck until you reveal a Treasure. Discard it or trash it. Discard the other cards.	\N	t	\N	\N	\N	\N	\N	t
113	trade-route	Prosperity	3	f	0	+1 Buy. Trash a card from your hand. +1 Coin per Coin token on the Trade Route mat.	Setup: Add a Coin token to each Victory Supply pile; move that token to the Trade Route mat when a card is gained from the pile.	t	Never	Never	Never	Never	Always	t
114	watchtower	Prosperity	3	f	0	Draw until you have 6 cards in hand.	When you gain card, you may reveal this from your hand, to either trash that card or put it onto your deck.	t	Never	Never	Sometimes	Sometimes	Never	f
115	bishop	Prosperity	4	f	0	+1 Coin. +1 VP Token. Trash a card from your hand. +1 VP Token per 2 Coins it costs (round down). Each other player may trash a card from their hand.	\N	t	Never	Never	Never	Never	Never	t
116	monument	Prosperity	4	f	0	+2 Coins. +1 VP Token.	\N	t	Never	Never	Never	Never	Never	f
117	quarry	Prosperity	4	f	0	1 Coin	While this is in play, Action cards cost 2 Coins less, but not less than 0 Coins.	t	\N	\N	\N	\N	\N	f
118	talisman	Prosperity	4	f	0	1 Coin	While this is in play, when you buy a non-Victory card costing 4 Coins or less, gain a copy of it.	t	\N	\N	\N	\N	\N	f
120	city	Prosperity	5	f	0	+1 Card. +2 Actions. If there are one or more empty Supply piles, +1 Card. If there are two or more, +1 Buy and +1 Coin.	\N	t	Always	Always	Always	Sometimes	Sometimes	f
121	contraband	Prosperity	5	f	0	3 Coins. +1 Buy. When you play this, the player to your left names a card. You can't buy that card this turn.	\N	t	\N	\N	\N	\N	Always	f
119	worker's-village	Prosperity	4	f	0	+1 Card. +2 Actions. +1 Buy.	\N	t	Always	Always	Always	Never	Always	f
122	counting-house	Prosperity	5	f	0	Look through your discard pile, reveal any number of Coppers from it, and put them into your hand.	\N	t	Never	Never	Sometimes	Sometimes	Never	f
123	mint	Prosperity	5	f	0	You may reveal a Treasure card from your hand. Gain a copy of it.	When you buy this, trash all Treasures you have in play.	t	Never	Never	Never	Never	Never	t
124	mountebank	Prosperity	5	f	0	+2 Coins. Each other player may discard a Curse. If they don't, they gain a Curse and a Copper.	\N	t	Never	Never	Never	Never	Never	f
125	rabble	Prosperity	5	f	0	+3 Cards. Each other player reveals the top 3 cards of their deck, discards the Actions and Treasures, and puts the rest back in any order they choose.	\N	t	Never	Never	Always	Always	Never	f
126	royal-seal	Prosperity	5	f	0	2 Coins	While this is in play, when you gain a card, you may put that card onto your deck.	t	\N	\N	\N	\N	\N	f
127	vault	Prosperity	5	f	0	2 Cards. Discard any number of cards for +1 Coin each. Each other player may discard 2 cards, to draw a card.	\N	t	Never	Never	Sometimes	Sometimes	Never	f
128	venture	Prosperity	5	f	0	1 Coin. When you play this, reveal cards from your deck until you reveal a Treasures. Discard the other cards. Play that Treasure.	\N	t	\N	\N	\N	\N	\N	f
140	hamlet	Cornucopia	2	f	0	+1 Card. +1 Action. You may discard a card for +1 Action. You may discard a card for +1 Buy.	\N	t	Always	Sometimes	Sometimes	Never	Sometimes	f
130	hoard	Prosperity	6	f	0	+2 Coins	While this is in play, when you buy a Victory card, gain a Gold.	t	\N	\N	\N	\N	\N	f
129	grand-market	Prosperity	6	f	0	+1 Card. +1 Action. +1 Buy. +2 Coins.	You can't buy this if you have any Coppers in play.	t	Always	Never	Always	Never	Always	f
131	grand-market	Prosperity	6	f	0	+1 Card. +1 Action. +1 Buy. +2 Coins.	You can't buy this if you have any Coppers in play.	t	Always	Never	Always	Never	Always	f
132	goons	Prosperity	6	f	0	+1 Buy. +2 Coins. Each other player discards down to 3 cards in hand.	While this is in play, when you buy a card, +1 VP Token.	t	Never	Never	Never	Never	Always	f
133	bank	Prosperity	7	f	0	When you play this, it's worth 1 Coin per Treasure you have in play (counting this).	\N	t	\N	\N	\N	\N	\N	f
134	expand	Prosperity	7	f	0	Trash a card from your hand. Gain a card costing up to 3 Coins more than it.	\N	t	Never	Never	Never	Never	Never	t
135	forge	Prosperity	7	f	0	Trash any number of cards from your hand. Gain a card with cost exactly equal to the total cost in Coins of the trashed cards.	\N	t	Never	Never	Never	Never	Never	t
136	king's-court	Prosperity	7	f	0	You may play an Action card from your hand three times.	\N	t	Sometimes	Sometimes	Sometimes	Sometimes	Sometimes	f
137	peddler	Prosperity	8	f	0	+1 Card. +1 Action. +1 Coin.	During your Buy phase, this costs 2 Coins less per Action card you have in play, but not less than 0 Coins.	t	Always	Never	Always	Never	Never	f
138	platinum	Prosperity	9	f	0	5 Coins	\N	f	\N	\N	\N	\N	\N	f
139	colony	Prosperity	11	f	0	10 VP	\N	f	\N	\N	\N	\N	\N	f
141	fortune-teller	Cornucopia	3	f	0	+2 Coins. Each other player reveals cards from the top of their deck until they reveal a Victory card or a Curse. They put it on top and discard the rest.	\N	t	Never	Never	Never	Never	Never	f
142	menagerie	Cornucopia	3	f	0	+1 Action. Reveal your hand. If the revealed cards all have different names, +3 Cards. Otherwise, +1 Card.	\N	t	Always	Never	Always	Sometimes	Never	f
143	farming-village	Cornucopia	4	f	0	+2 Actions. Reveal cards from your deck until you reveal a Treasure or Action card. Put that card into your hand and discard the rest.	\N	t	Always	Always	Always	Never	Never	f
144	horse-traders	Cornucopia	4	f	0	+1 Buy. +3 Coins. Discard 2 cards.	When another player plays an Attack card, you may first set this aside from your hand. If you do, then at the start of your next turn, +1 Card and return this to your hand.	t	Never	Never	Never	Never	Always	f
145	remake	Cornucopia	4	f	0	Do this twice: Trash a card from your hand, then gain a card costing exactly 1 Coin more than it.	\N	t	Never	Never	Never	Never	Never	t
147	bag-of-gold	Cornucopia	0	f	0	+1 Action. Gain a Gold onto your deck. (This is not in the Supply.)	\N	f	Always	Never	Never	Never	Never	f
148	diadem	Cornucopia	0	f	0	2 Coins. When you play this, +1 Coin per unused Action you have (Action, not Action card). (This is not in the Supply.)	\N	f	\N	\N	\N	\N	\N	f
149	followers	Cornucopia	0	f	0	+2 Cards. Gain an Estate. Each other player gains a Curse and discards down to 3 cards in hand. (This is not in the Supply.)	\N	f	Never	Never	Always	Always	Never	f
150	princess	Cornucopia	0	f	0	+1 Buy	While this is in play, cards cost 2 Coins less, but not less than 0 Coins. (This is not in the Supply.)	f	Never	Never	Never	Never	Always	f
151	trusty-steed	Cornucopia	0	f	0	Choose two: +2 Cards; or +2 Actions; or +2 Coins; or gain 4 Silvers and put your deck into your discard pile. The choices must be different. (This is not in the Supply.)	\N	f	Sometimes	Sometimes	Sometimes	Sometimes	Never	f
152	young-witch	Cornucopia	4	f	0	+2 Cards. Discard 2 cards. Each other player may reveal a Bane card from their hand; if they don't, they gain a Curse.	Setup: Add an extra Kingdom card pile costing 2 Coins or 3 Coins to the Supply. Cards from that pile are Bane cards.	t	Never	Never	Never	Never	Never	f
153	harvest	Cornucopia	5	f	0	Reveal the top 4 cards of your deck, then discard them. +1 Coin per differently named card revealed.	\N	t	Never	Never	Never	Never	Never	f
154	horn-of-plenty	Cornucopia	5	f	0	When you play this, gain a card costing up to 1 Coin per differently named card you have in play (counting this). If it's a Victory card, trash this.	\N	t	\N	\N	\N	\N	\N	f
155	hunting-party	Cornucopia	5	f	0	+1 Card. +1 Action. Reveal your hand. Reveal cards from your deck until you reveal one that isn't a copy of one in your hand. Put it into your hand and discard the rest.	\N	t	Always	Never	Always	Sometimes	Never	f
156	jester	Cornucopia	5	f	0	+2 Coins. Each other player discards the top card of their deck. If it's a Victory card they gain a Curse; otherwise they gain a copy of the discarded card or you do, your choice.	\N	t	Never	Never	Never	Never	Never	f
157	fairgrounds	Cornucopia	6	f	0	Worth 2 VP per 5 differently named cards you have (round down).	\N	t	\N	\N	\N	\N	\N	f
158	crossroads	Hinterlands	2	f	0	Reveal your hand. +1 Card per Victory card revealed. If this is the first time you played a Crossroads this turn, +3 Actions.	\N	t	Sometimes	Sometimes	Sometimes	Sometimes	Never	f
159	duchess	Hinterlands	2	f	0	+2 Coins. Each player (including you) looks at the top card of their deck and may discard it.	In games using this, when you gain a Duchy, you may gain a Duchess.	t	Never	Never	Never	Never	Never	f
160	fool's-gold	Hinterlands	2	f	0	Worth 1 Coin if it's the first time you played a Fool's Gold this turn, otherwise worth 4 Coins.	When another player gains a Province, you may trash this from your hand, to gain a Gold onto your deck.	t	\N	\N	\N	\N	\N	f
161	develop	Hinterlands	3	f	0	Trash a card from your hand. Gain two cards onto your deck, with one costing exactly 1 Coin more than it, and one costing exactly 1 Coin less than it, in either order.	\N	t	Never	Never	Never	Never	Never	t
162	oasis	Hinterlands	3	f	0	+1 Card. +1 Action. +1 Coin. Discard a card.	\N	t	Always	Never	Never	Never	Never	f
163	oracle	Hinterlands	3	f	0	Each player (including you) reveals the top 2 cards of their deck, and discards them or puts them back, your choice. They choose the order to return them. Afterwards, you draw 2 cards.	\N	t	Never	Never	Always	Always	Never	f
164	scheme	Hinterlands	3	f	0	+1 Card. +1 Action. This turn, you may put one of your Action cards onto your deck when you discard it from play.	\N	t	Always	Never	Always	Never	Never	f
165	tunnel	Hinterlands	3	f	0	2 VP	When you discard this other than during Clean-up, you may reveal it to gain a Gold.	t	\N	\N	\N	\N	\N	f
167	noble-brigand	Hinterlands	4	f	0	+1 Coin. When you buy or play this, each other player reveals the top 2 cards of their deck, trashes a revealed Silver or Gold you choose, discards the rest, and gains a Copper if they didn't reveal a Treasure. You gain the trashed cards.	\N	t	Never	Never	Never	Never	Never	f
166	jack-of-all-trades	Hinterlands	4	f	0	Gain a Silver. Look at the top card of your deck; you may discard it. Draw until you have 5 cards in hand. You may trash a non-Treasure from your hand.	\N	t	Never	Never	Sometimes	Sometimes	Never	t
168	nomad-camp	Hinterlands	4	f	0	+1 Buy. +2 Coins.	This is gained onto your deck (instead of to your discard pile).	t	Never	Never	Never	Never	Always	f
169	silk-road	Hinterlands	4	f	0	Worth 1 VP for every 4 Victory cards you have (round down).	\N	t	\N	\N	\N	\N	\N	f
170	spice-merchant	Hinterlands	4	f	0	You may trash a Treasure from your hand to choose one: +2 Cards and +1 Action; or +1 Buy and +2 Coins.	\N	t	Sometimes	Never	Sometimes	Never	Sometimes	t
171	cache	Hinterlands	5	f	0	3 Coins	When you gain this, gain 2 Coppers.	t	\N	\N	\N	\N	\N	f
172	trader	Hinterlands	4	f	0	Trash a card from your hand. Gain a Silver per 1 Coin it costs.	When you would gain a card, you may reveal this from your hand, to instead gain a Silver.	t	Never	Never	Never	Never	Never	t
173	cartographer	Hinterlands	5	f	0	+1 Card. +1 Action. Look at the top 4 cards of your deck. Discard any number of them, then put the rest back in any order.	\N	t	Always	Never	Always	Never	Never	f
174	embassy	Hinterlands	5	f	0	+5 Cards. Discard 3 cards.	When you gain this, each other player gains a Silver.	t	Never	Never	Always	Always	Never	f
175	haggler	Hinterlands	5	f	0	+2 Coins	While this is in play, when you buy a card, gain a cheaper non-Victory card.	t	Never	Never	Never	Never	Never	f
176	highway	Hinterlands	5	f	0	+1 Card. +1 Action.	While this is in play, cards cost 1 Coin less, but not less than 0 Coins.	t	Always	Never	Always	Never	Never	f
177	ill-gotten-gains	Hinterlands	5	f	0	1 Coin. When you play this, you may gain a Copper to your hand.	When you gain this, each other player gains a Curse.	t	\N	\N	\N	\N	\N	f
178	inn	Hinterlands	5	f	0	+2 Cards. +2 Actions. Discard 2 cards.	When you gain this, look through your discard pile, reveal any number of Action cards from it (which can include this), and shuffle them into your deck.	t	Always	Always	Always	Never	Never	f
179	mandarin	Hinterlands	5	f	0	+3 Coins. Put a card from your hand onto your deck.	When you gain this, put all Treasures you have in play onto your deck in any order.	t	Never	Never	Never	Never	Never	f
180	margrave	Hinterlands	5	f	0	+3 Cards. +1 Buy. Each other player draws a card, then discards down to 3 cards in hand.	\N	t	Never	Never	Always	Always	Always	f
181	stables	Hinterlands	5	f	0	You may discard a Treasure, for +3 Cards and +1 Action.	\N	t	Sometimes	Never	Sometimes	Sometimes	Never	f
182	border-village	Hinterlands	6	f	0	+1 Card. +2 Actions.	When you gain this, gain a cheaper card.	t	Always	Always	Always	Never	Never	f
183	farmland	Hinterlands	6	f	0	2 VP	When you buy this, trash a card from your hand and gain a card costing exactly 2 Coins more than it.	t	\N	\N	\N	\N	\N	t
184	abandoned-mine	DarkAges	0	f	0	+1 Coin	\N	f	Never	Never	Never	Never	Never	f
185	ruined-library	DarkAges	0	f	0	+1 Card	\N	f	Never	Never	Always	Never	Never	f
186	ruined-market	DarkAges	0	f	0	+1 Buy	\N	f	Never	Never	Never	Never	Always	f
187	ruined-village	DarkAges	0	f	0	+1 Action	\N	f	Always	Never	Never	Never	Never	f
188	survivors	DarkAges	0	f	0	Look at the top 2 cards of your deck. Discard them or put them back in any order.	\N	f	Never	Never	Never	Never	Never	f
189	spoils	DarkAges	0	f	0	3 Coins. When you play this, return it to the Spoils pile. (This is not in the Supply.)	\N	f	\N	\N	\N	\N	\N	f
190	poor-house	DarkAges	1	f	0	+4 Coins. Reveal your hand. -1 Coin per Treasure card in your hand. (You can't go below 0 Coins.)	\N	t	Never	Never	Never	Never	Never	f
191	hovel	DarkAges	1	f	0	When you buy a Victory card, you may trash this from your hand.	\N	f	\N	\N	\N	\N	\N	f
192	necropolis	DarkAges	1	f	0	+2 Actions	\N	f	Always	Always	Never	Never	Never	f
193	overgrown-estate	DarkAges	1	f	0	0 VP	When you trash this, +1 Card.	f	\N	\N	\N	\N	\N	f
194	beggar	DarkAges	2	f	0	Gain 3 Coppers to your hand.	When another player plays an Attack card, you may first discard this to gain 2 Silvers, putting one onto your deck.	t	Never	Never	Always	Always	Never	f
195	squire	DarkAges	2	f	0	+1 Coin. Choose one: +2 Actions; or +2 Buys; or gain a Silver.	When you trash this, gain an Attack card.	t	Sometimes	Sometimes	Never	Never	Sometimes	f
196	vagrant	DarkAges	2	f	0	+1 Card. +1 Action. Reveal the top card of your deck. If it's a Curse, Ruins, Shelter, or Victory card, put it into your hand.	\N	t	Always	Never	Always	Sometimes	Never	f
197	forager	DarkAges	3	f	0	+1 Action. +1 Buy. Trash a card from your hand, then +1 Coin per differently named Treasure in the trash.	\N	t	Always	Never	Never	Never	Always	t
198	hermit	DarkAges	3	f	0	Look through your discard pile. You may trash a non-Treasure card from your discard pile or hand. Gain a card costing up to 3 Coins.	When you discard this from play, if you didn't buy any cards this turn, trash this and gain a Madman from the Madman pile.	t	Never	Never	Never	Never	Never	t
199	madman	DarkAges	0	f	0	+2 Actions. Return this to the Madman pile. If you do, +1 Card per card in your hand. (This is not in the Supply.)	\N	f	Always	Always	Sometimes	Sometimes	Never	f
200	market-square	DarkAges	3	f	0	+1 Card. +1 Action. +1 Buy.	When one of your cards is trashed, you may discard this from your hand to gain a Gold.	t	Always	Never	Always	Never	Always	f
201	sage	DarkAges	3	f	0	+1 Action. Reveal cards from the top of your deck until you reveal one costing 3 Coins or more. Put that card into your hand and discard the rest.	\N	t	Always	Never	Sometimes	Never	Never	f
202	storeroom	DarkAges	3	f	0	+1 Buy. Discard any number of cards, then draw that many. Then discard any number of cards for +1 Coin each.	\N	t	Never	Never	Never	Never	Always	f
203	urchin	DarkAges	3	f	0	+1 Card. +1 Action. Each other player discards down to 4 cards in hand.	When you play another Attack card with this in play, you may first trash this, to gain a Mercenary from the Mercenary pile.	t	Always	Never	Always	Never	Never	f
204	mercenary	DarkAges	0	f	0	You may trash 2 cards from your hand. If you did, +2 Cards, +2 Coins, and each other player discards down to 3 cards in hand. (This is not in the Supply.)	\N	f	Never	Never	Never	Never	Never	t
205	armory	DarkAges	4	f	0	Gain a card onto your deck costing up to 4 Coins.	\N	t	Never	Never	Never	Never	Never	f
206	death-cart	DarkAges	4	f	0	+5 Coins. You may trash an Action card from your hand. If you don't, trash this.	When you gain this, gain 2 Ruins.	t	Never	Never	Never	Never	Never	t
207	feodum	DarkAges	4	f	0	Worth 1 VP per 3 Silvers you have (round down).	When you trash this, gain 3 Silvers.	t	\N	\N	\N	\N	\N	f
208	fortress	DarkAges	4	f	0	+1 Card. +2 Actions.	When you trash this, put it into your hand.	t	Always	Always	Always	Never	Never	f
209	ironmonger	DarkAges	4	f	0	+1 Card. +1 Action. Reveal the top card of your deck; you may discard it. Either way, if it is an... Action card, +1 Action. Treasure card, +1 Coin. Victory card, +1 Card.	\N	t	Always	Sometimes	Always	Sometimes	Never	f
210	marauder	DarkAges	4	f	0	Gain a Spoils from the Spoils pile. Each other player gains a Ruins.	\N	t	Never	Never	Never	Never	Never	f
211	procession	DarkAges	4	f	0	You may play an Action card from your hand twice. Trash it. Gain an Action card costing exactly 1 Coin more than it.	\N	t	Sometimes	Sometimes	Sometimes	Sometimes	Sometimes	t
212	rats	DarkAges	4	f	0	+1 Card. +1 Action. Gain a Rats. Trash a card from your hand other than a Rats (or reveal a hand of all Rats).	When you trash this, +1 Card.	t	Always	Never	Sometimes	Never	Never	t
213	scavenger	DarkAges	4	f	0	+2 Coins. You may put your deck into your discard pile. Look through your discard pile and put one card from it onto your deck.	\N	t	Never	Never	Never	Never	Never	f
214	wandering-minstrel	DarkAges	4	f	0	+1 Card. +2 Actions. Reveal the top 3 cards of your deck. Put the Action cards back in any order and discard the rest.	\N	t	Always	Always	Always	Never	Never	f
244	plaza	Guilds	4	f	0	+1 Card. +2 Actions. You may discard a Treasure for +1 Coffers.	\N	t	Always	Always	Sometimes	Never	Never	f
215	band-of-misfits	DarkAges	5	f	0	Play this as if it were a cheaper Action card in the Supply. This is that card until it leaves play.	\N	t	Sometimes	Sometimes	Sometimes	Sometimes	Sometimes	f
216	bandit-camp	DarkAges	5	f	0	+1 Card. +2 Actions. Gain a Spoils from the Spoils pile.	\N	t	Always	Always	Always	Never	Never	f
217	catacombs	DarkAges	5	f	0	Look at the top 3 cards of your deck. Choose one: Put them into your hand; or discard them and +3 Cards.	When you trash this, gain a cheaper card.	t	Never	Never	Always	Always	Never	f
218	count	DarkAges	5	f	0	Choose one: Discard 2 cards; or put a card from your hand onto your deck; or gain a Copper. Choose one: +3 Coins; or trash your hand; or gain a Duchy.	\N	t	Never	Never	Never	Never	Never	t
219	counterfeit	DarkAges	5	f	0	1 Coin. +1 Buy. When you play this, you may play a Treasure from your hand twice. If you do, trash that Treasure.	\N	t	\N	\N	\N	\N	Always	t
220	cultist	DarkAges	5	f	0	+2 Cards. Each other player gains a Ruins. You may play a Cultist from your hand.	When you trash this, +3 Cards	t	Never	Never	Always	Always	Never	f
221	graverobber	DarkAges	5	f	0	Choose one: Gain a card from the trash costing from 3 Coins to 6 Coins, onto your deck; or trash an Action card from your hand and gain a card costing up to 3 Coins more than it.	\N	t	Never	Never	Never	Never	Never	t
222	junk-dealer	DarkAges	5	f	0	+1 Card. +1 Action. +1 Coin. Trash a card from your hand.	\N	t	Always	Never	Never	Never	Never	t
223	dame-anna	DarkAges	5	f	0	You may trash up to 2 cards from your hand. Each other player reveals the top 2 cards of their deck, trashes one of them costing from 3 Coins to 6 Coins, and discards the rest. If a Knight is trashed by this, trash this.	\N	t	Never	Never	Never	Never	Never	t
225	dame-molly	DarkAges	5	f	0	+2 Actions. Each other player reveals the top 2 cards of their deck, trashes one of them costing from 3 Coins to 6 Coins, and discards the rest. If a Knight is trashed by this, trash this.	\N	t	Always	Always	Never	Never	Never	f
224	dame-josephine	DarkAges	5	f	0	Each other player reveals the top 2 cards of their deck, trashes one of them costing from 3 Coins to 6 Coins, and discards the rest. If a Knight is trashed by this, trash this.	2 VP	t	Never	Never	Never	Never	Never	f
226	dame-natalie	DarkAges	5	f	0	You may gain a card costing up to 3 Coins. Each other player reveals the top 2 cards of their deck, trashes one of them costing from 3 Coins to 6 Coins, and discards the rest. If a Knight is trashed by this, trash this.	\N	t	Never	Never	Never	Never	Never	f
227	dame-sylvia	DarkAges	5	f	0	+2 Coins. Each other player reveals the top 2 cards of their deck, trashes one of them costing from 3 Coins to 6 Coins, and discards the rest. If a Knight is trashed by this, trash this.	\N	t	Never	Never	Never	Never	Never	f
228	sir-bailey	DarkAges	5	f	0	+1 Card. +1 Action. Each other player reveals the top 2 cards of their deck, trashes one of them costing from 3 Coins to 6 Coins, and discards the rest. If a Knight is trashed by this, trash this.	\N	t	Always	Never	Always	Never	Never	f
229	sir-destry	DarkAges	5	f	0	+2 Cards. Each other player reveals the top 2 cards of their deck, trashes one of them costing from 3 Coins to 6 Coins, and discards the rest. If a Knight is trashed by this, trash this.	\N	t	Never	Never	Always	Always	Never	f
230	sir-martin	DarkAges	4	f	0	+2 Buys. Each other player reveals the top 2 cards of their deck, trashes one of them costing from 3 Coins to 6 Coins, and discards the rest. If a Knight is trashed by this, trash this.	\N	t	Never	Never	Never	Never	Always	f
231	sir-michael	DarkAges	5	f	0	Each other player discards down to 3 cards in hand. Each other player reveals the top 2 cards of their deck, trashes one of them costing from 3 Coins to 6 Coins, and discards the rest. If a Knight is trashed by this, trash this.	\N	t	Never	Never	Never	Never	Never	f
232	sir-vander	DarkAges	5	f	0	Each other player reveals the top 2 cards of their deck, trashes one of them costing from 3 Coins to 6 Coins, and discards the rest. If a Knight is trashed by this, trash this.	When you trash this, gain a Gold	t	Never	Never	Never	Never	Never	f
233	mystic	DarkAges	5	f	0	+1 Action. +2 Coins. Name a card, then reveal the top card of your deck. If you named it, put it into your hand.	\N	t	Always	Never	Sometimes	Never	Never	f
234	pillage	DarkAges	5	f	0	Trash this. Each other player with 5 or more cards in hand reveals their hand and discards a card that you choose. Gain 2 Spoils from the Spoils pile.	\N	t	Never	Never	Never	Never	Never	f
235	rebuild	DarkAges	5	f	0	+1 Action. Name a card, Reveal cards from your deck until you reveal a Victory card you did not name. Discard the rest, trash the Victory card, and gain a Victory card costing up to 3 Coins more than it.	\N	t	Always	Never	Never	Never	Never	t
236	rogue	DarkAges	5	f	0	+2 Coins. If there are any cards in the trash costing from 3 Coins to 6 Coins, gain one of them. Otherwise, each other player reveals the top 2 cards of their deck, trashes one of them costing from 3 Coins to 6 Coins, and discards the rest.	\N	t	Never	Never	Never	Never	Never	f
237	altar	DarkAges	6	f	0	Trash a card from your hand. Gain a card costing up to 5 Coins.	\N	t	Never	Never	Never	Never	Never	t
238	hunting-grounds	DarkAges	6	f	0	+4 Cards	When you trash this, gain a Duchy or 3 Estates.	t	Never	Never	Always	Always	Never	f
240	stonemason	Guilds	2	f	0	Trash a card from your hand. Gain 2 cards each costing less than it.	When you buy this, you may overpay for it. If you do, gain 2 Action cards each costing the amount you overpaid.	t	Never	Never	Never	Never	Never	t
239	candlestick-maker	Guilds	2	f	0	+1 Action. +1 Buy. +1 Coffers.	\N	t	Always	Never	Never	Never	Always	f
241	doctor	Guilds	3	f	0	Name a card. Reveal the top 3 cards of your deck. Trash the matches. Put the rest back in any order.	When you buy this, you may overpay for it. For each 1 Coin you overpaid, look at the top card of your deck; trash it, discard it, or put it back.	t	Never	Never	Never	Never	Never	t
242	masterpiece	Guilds	3	f	0	1 Coin	When you buy this, you may overpay for it. For each 1 Coin you overpaid, gain a Silver.	t	\N	\N	\N	\N	\N	f
243	advisor	Guilds	4	f	0	+1 Action. Reveal the top 3 cards of your deck. The player to your left chooses one of them. Discard that card and put the rest into your hand.	\N	t	Always	Never	Always	Always	Never	f
245	taxman	Guilds	4	f	0	You may trash a Treasure from your hand. Each other player with 5 or more cards in hand discards a copy of it (or reveals they can't). Gain a Treasure onto your deck costing up to 3 Coins more than it.	\N	t	Never	Never	Never	Never	Never	t
246	herald	Guilds	4	f	0	+1 Card. +1 Action. Reveal the top card of your deck. If it's an Action, play it.	When you buy this, you may overpay for it. For each 1 Coin you overpaid, look through your discard pile and put a card from it onto your deck.	t	Always	Sometimes	Always	Sometimes	Sometimes	f
247	baker	Guilds	5	f	0	+1 Card. +1 Action. +1 Coffers.	Setup: Each player gets +1 Coffers.	t	Always	Never	Always	Never	Never	f
248	butcher	Guilds	5	f	0	+2 Coffers. You may trash a card from your hand. If you do, remove any number of tokens from your Coffers and gain a card, costing up to the cost of the trashed card plus 1 Coin per token removed.	\N	t	Never	Never	Never	Never	Never	t
249	journeyman	Guilds	5	f	0	Name a card. Reveal cards from your deck until you reveal 3 cards without that name. Put those cards into your hand and discard the rest.	\N	t	Never	Never	Always	Always	Never	f
250	merchant-guild	Guilds	5	f	0	+1 Buy. +1 Coin.	While this is in play, when you buy a card, +1 Coffers.	t	Never	Never	Never	Never	Always	f
251	soothsayer	Guilds	5	f	0	Gain a Gold. Each other player gains a Curse, and if they did, draws a card.	\N	t	Never	Never	Never	Never	Never	f
252	coin-of-the-realm	Adventures	2	f	0	+1 Coin. When you play this, put it on your Tavern mat.	Directly after you finish playing an Action card, you may call this, for +2 Actions.	t	\N	\N	\N	\N	\N	f
253	page	Adventures	2	f	0	+1 Card. +1 Action.	When you discard this from play, you may exchange it for a Treasure Hunter.	t	Always	Never	Always	Never	Never	f
254	treasure-hunter	Adventures	3	f	0	+1 Action. +1 Coin. Gain a Silver per card the player to your right gained on their last turn.	When you discard this from play, you may exchange it for a Warrior. (This is not in the Supply.)	f	Always	Never	Never	Never	Never	f
255	warrior	Adventures	4	f	0	+2 Cards. Once per Traveller you have in play (including this), each other player discards the top card of their deck and trashes it if it costs 3 Coins or 4 Coins.	When you discard this from play, you may exchange it for a Hero. (This is not in the Supply.)	f	Never	Never	Always	Always	Never	f
256	hero	Adventures	5	f	0	+2 Coins. Gain a Treasure.	When you discard this from play, you may exchange it for a Champion. (This is not in the Supply.)	f	Never	Never	Never	Never	Never	f
257	champion	Adventures	6	f	0	+1 Action. For the rest of the game, when another player plays an Attack card, it doesn't affect you, and when you play an Action, +1 Action. (This stays in play. This is not in the Supply.)	\N	f	Always	Never	Never	Never	Never	f
258	peasant	Adventures	2	f	0	+1 Buy. +1 Coin.	When you discard this from play, you may exchange it for a Soldier.	t	Never	Never	Never	Never	Always	f
259	soldier	Adventures	3	f	0	+2 Coins. +1 Coin per other Attack you have in play. Each other player with 4 or more cards in hand discards a card.	When you discard this from play, you may exchange it for a Fugitive. (This is not in the Supply.)	f	Never	Never	Never	Never	Never	f
260	fugitive	Adventures	4	f	0	+2 Cards. +1 Action. Discard a card.	When you discard this from play, you may exchange it for a Disciple. (This is not in the Supply.)	f	Always	Never	Always	Never	Never	f
261	disciple	Adventures	5	f	0	You may play an Action card from your hand twice. Gain a copy of it.	When you discard this from play, you may exchange it for a Teacher. (This is not in the Supply.)	f	Sometimes	Sometimes	Sometimes	Sometimes	Sometimes	f
262	teacher	Adventures	6	f	0	Put this on your Tavern mat.	At the start of your turn, you may call this, to move your +1 Card, +1 Action, +1 Buy, or +1 Coin token to an Action Supply pile you have no tokens on. (When you play a card from that pile, you first get that bonus.) (This is not in the Supply.)	f	Never	Never	Never	Never	Never	f
263	ratcatcher	Adventures	2	f	0	+1 Card. +1 Action. Put this on your Tavern mat.	At the start of your turn, you may call this, to trash a card from your hand.	t	Always	Never	Always	Never	Never	t
264	raze	Adventures	2	f	0	+1 Action. Trash this or a card from your hand. Look at one card from the top of your deck per 1 Coin the trashed card costs. Put one of them into your hand and discard the rest.	\N	t	Always	Never	Never	Never	Never	t
265	amulet	Adventures	3	f	0	Now and at the start of your next turn, choose one: +1 Coin; or trash a card from your hand; or gain a Silver.	\N	t	Never	Never	Never	Never	Never	t
266	caravan-guard	Adventures	3	f	0	+1 Card. +1 Action. At the start of your next turn, +1 Coin.	When another player plans an Attack card, you may first play this from your hand. (+1 Action has no effect if it's not your turn.)	t	Always	Never	Always	Never	Never	f
267	dungeon	Adventures	3	f	0	+1 Action. Now and at the start of your next turn: +2 Cards, then discard 2 cards.	\N	t	Always	Never	Never	Never	Never	f
268	gear	Adventures	3	f	0	+2 Cards. Set aside up to 2 cards from your hand face down (under this). At the start of your next turn, put them into your hand.	\N	t	Never	Never	Sometimes	Sometimes	Never	f
269	guide	Adventures	3	f	0	+1 Card. +1 Action. Put this on your Tavern mat.	At the start of your turn, you may call this, to discard your hand and draw 5 cards.	t	Always	Never	Always	Never	Never	f
270	duplicate	Adventures	4	f	0	Put this on your Tavern mat.	When you gain a card costing up to 6 Coins, you may call this, to gain a copy of that card.	t	Never	Never	Never	Never	Never	f
271	magpie	Adventures	4	f	0	+1 Card. +1 Action. Reveal the top card of your deck. If it's a Treasure, put it into your hand. If it's an Action or Victory card, gain a Magpie.	\N	t	Always	Never	Always	Sometimes	Never	f
272	messenger	Adventures	4	f	0	+1 Buy. +2 Coins. You may put your deck into your discard pile.	When this is your first buy in a turn, gain a card costing up to 4 Coins, and each other player gains a copy of it.	t	Never	Never	Never	Never	Always	f
273	miser	Adventures	4	f	0	Choose one: Put a Copper from your hand onto your Tavern mat; or +1 Coin per Copper on your Tavern mat.	\N	t	Never	Never	Never	Never	Never	t
274	port	Adventures	4	f	0	+1 Card. +2 Actions.	When you buy this, gain another Port.	t	Always	Always	Always	Never	Never	f
275	ranger	Adventures	4	f	0	+1 Buy. Turn your Journey token over (it starts face up). Then if it's face up, +5 Cards.	\N	t	Never	Never	Sometimes	Sometimes	Always	f
276	transmogrify	Adventures	4	f	0	+1 Action. Put this on your Tavern mat.	At the start of your turn, you may call this, to trash a card from your hand, and gain a card to your hand costing up to 1 Coin more than it.	t	Always	Never	Never	Never	Never	t
277	artificer	Adventures	5	f	0	+1 Card. +1 Action. +1 Coin. Discard any number of cards. You may gain a card onto your deck costing exactly 1 Coin per card discarded.	\N	t	Always	Never	Sometimes	Never	Never	f
278	bridge-troll	Adventures	5	f	0	Each other player takes their -1 Coin token. Now and at the start of your next turn: +1 Buy.	While this is in play, cards cost 1 Coin less on your turns, but not less than 0 Coins.	t	Never	Never	Never	Never	Always	f
279	distant-lands	Adventures	5	f	0	Put this on your Tavern mat.	Worth 4 VP if on your Tavern mat at the end of the game (otherwise worth 0 VP).	t	Never	Never	Never	Never	Never	f
280	giant	Adventures	5	f	0	Turn your Journey token over (it starts face up). Then if it's face down, +1 Coin. If it's face up, +5 Coins, and each other player reveals the top card of their deck, trashes it if it costs from 3 Coins to 6 Coins, and otherwise discards it and gains a Curse.	\N	t	Never	Never	Never	Never	Never	f
281	haunted-woods	Adventures	5	f	0	Until your next turn, when any other player buys a card, they put their hand onto their deck in any order. At the start of your next turn: +3 Cards.	\N	t	Never	Never	Never	Never	Never	f
282	lost-city	Adventures	5	f	0	+2 Cards. +2 Actions.	When you gain this, each other player draws a card.	t	Always	Always	Always	Always	Never	f
283	relic	Adventures	5	f	0	2 Coins. When you play this, each other player puts their -1 Card token on their deck.	\N	t	\N	\N	\N	\N	\N	f
284	royal-carriage	Adventures	5	f	0	+1 Action. Put this on your Tavern mat.	Directly after you finish playing an Action card, if it's still in play, you may call this, to replay that Action.	t	Always	Never	Never	Never	Never	f
285	storyteller	Adventures	5	f	0	+1 Action. +1 Coin. Play up to 3 Treasures from your hand. Then pay all of your Coins (including the 1 Coin from this) and draw a card per 1 Coin you paid.	\N	t	Always	Never	Always	Sometimes	Never	f
286	swamp-hag	Adventures	5	f	0	Until your next turn, when any other player buys a card, they gain a Curse. At the start of your next turn: +3 Coins.	\N	t	Never	Never	Never	Never	Never	f
287	treasure-trove	Adventures	5	f	0	2 Coins. When you play this, gain a Gold and a Copper.	\N	t	\N	\N	\N	\N	\N	f
288	wine-merchant	Adventures	5	f	0	+4 Coins. +1 Buy. Put this on your Tavern mat.	At the end of your Buy phase, if you have at least 2 Coins unspent, you may discard this from your Tavern mat.	t	Never	Never	Never	Never	Always	f
289	hireling	Adventures	6	f	0	At the start of each of your turns for the rest of the game: +1 Card. (This stays in play.)	\N	t	Never	Never	Never	Never	Never	f
290	alms	Adventures	0	f	0	Once per turn: If you have no Treasures in play, gain a card costing up to 4 Coins.	\N	f	\N	\N	\N	\N	\N	f
291	borrow	Adventures	0	f	0	Once per turn: +1 Buy. If your -1 Card token isn't on your deck, put it there and +1 Coin.	\N	f	\N	\N	\N	\N	Sometimes	f
292	quest	Adventures	0	f	0	You may discard an Attack, two Curses, or six cards. If you do, gain a Gold.	\N	f	\N	\N	\N	\N	\N	f
293	save	Adventures	1	f	0	Once per turn: +1 Buy. Set aside a card from your hand, and put it into your hand at end of turn (after drawing).	\N	f	\N	\N	\N	\N	Sometimes	f
294	scouting-party	Adventures	2	f	0	+1 Buy. Look at the top 5 cards of your deck. Discard 3 and put the rest back in any order.	\N	f	\N	\N	\N	\N	Always	f
295	travelling-fair	Adventures	2	f	0	+2 Buys. When you gain a card this turn, you may put it onto your deck.	\N	f	\N	\N	\N	\N	Always	f
296	bonfire	Adventures	3	f	0	Trash up to 2 cards you have in play.	\N	f	\N	\N	\N	\N	\N	t
297	expedition	Adventures	3	f	0	Draw 2 extra cards for your next hand.	\N	f	\N	\N	\N	\N	\N	f
298	ferry	Adventures	3	f	0	Move your -2 Coin cost token to an Action Supply pile. (Cards from that pile cost 2 Coins less on your turns, but not less than 0 Coins.)	\N	f	\N	\N	\N	\N	\N	f
299	plan	Adventures	3	f	0	Move your Trashing token to an Action Supply pile. (When you buy a card from that pile, you may trash a card from your hand.)	\N	f	\N	\N	\N	\N	\N	t
300	mission	Adventures	4	f	0	Once per turn: If the previous turn wasn't yours, take another turn after this one, during which you can't buy cards.	\N	f	\N	\N	\N	\N	\N	f
301	pilgrimage	Adventures	4	f	0	Once per turn: Turn your Journey token over (it starts face up); then if it's face up, choose up to 3 differently named cards you have in play and gain a copy of each.	\N	f	\N	\N	\N	\N	\N	f
302	ball	Adventures	5	f	0	Take your -1 Coin token. Gain 2 cards each costing up to 4 Coins.	\N	f	\N	\N	\N	\N	\N	f
303	raid	Adventures	5	f	0	Gain a Silver per Silver you have in play. Each other player puts their -1 Card token on their deck.	\N	f	\N	\N	\N	\N	\N	f
304	seaway	Adventures	5	f	0	Gain an Action card costing up to 4 Coins. Move your +1 Buy token to its pile. (When you play a card from that pile, you first get +1 Buy.)	\N	f	\N	\N	\N	\N	\N	f
305	trade	Adventures	5	f	0	Trash up to 2 cards from your hand. Gain a Silver per card you trashed.	\N	f	\N	\N	\N	\N	\N	t
306	lost-arts	Adventures	6	f	0	Move your +1 Action token to an Action Supply pile. (When you play a card from that pile, you first get +1 Action.)	\N	f	\N	\N	\N	\N	\N	f
307	training	Adventures	6	f	0	Move your +1 Coin token to an Action Supply pile. (When you play a card from that pile, you first get +1 Coin.)	\N	f	\N	\N	\N	\N	\N	f
308	inheritance	Adventures	7	f	0	Once per game: Set aside a non-Victory Action card from the Supply costing up to 4 Coins. Move your Estate token to it. (Your Estates gain the abilities and types of that card.)	\N	f	\N	\N	\N	\N	\N	f
309	pathfinding	Adventures	8	f	0	Move your +1 Card token to an Action Supply pile. (When you play a card from that pile, you first get +1 Card.)	\N	f	\N	\N	\N	\N	\N	f
310	engineer	Empires	0	f	4	Gain a card costing up to 4 Coins. You may trash this. If you do, gain a card costing up to 4 Coins.	\N	t	Never	Never	Never	Never	Never	f
311	city-quarter	Empires	0	f	8	+2 Actions. Reveal your hand. +1 Card per Action card revealed.	\N	t	Always	Always	Sometimes	Sometimes	Never	f
312	overlord	Empires	0	f	8	Play this as if it were an Action card in the Supply costing up to 5 Coins. This is that card until it leaves play.	\N	t	Sometimes	Sometimes	Sometimes	Sometimes	Sometimes	f
313	royal-blacksmith	Empires	0	f	8	+5 Cards. Reveal your hand; discard the Coppers.	\N	t	Never	Never	Sometimes	Sometimes	Never	f
314	encampment	Empires	2	f	0	+2 Cards. +2 Actions. You may reveal a Gold or Plunder from your hand. If you do not, set this aside, and return it to the Supply at the start of Clean-up.	\N	t	Always	Always	Always	Always	Never	f
315	plunder	Empires	5	f	0	2 Coins. +1 VP.	\N	t	\N	\N	\N	\N	\N	f
316	patrician	Empires	2	f	0	+1 Card. +1 Action. Reveal the top card of your deck. If it costs 5 Coins or more, put it into your hand.	\N	t	Always	Never	Always	Never	Never	f
317	emporium	Empires	5	f	0	+1 Card. +1 Action. +1 Coin.	When you gain this, if you have at least 5 Action cards in play, +2 VP.	t	Always	Never	Always	Never	Never	f
318	settlers	Empires	2	f	0	+1 Card. +1 Action. Look through your discard pile. You may reveal a Copper from it and put it into your hand.	\N	t	Always	Never	Always	Sometimes	Never	f
319	bustling-village	Empires	5	f	0	+1 Card. +3 Actions. Look through your discard pile. You may reveal a Settlers from it and put it into your hand.	\N	t	Always	Always	Always	Sometimes	Never	f
320	humble-castle	Empires	3	f	0	1 Coin	Worth 1 VP per Castle you have.	t	\N	\N	\N	\N	\N	f
321	crumbling-castle	Empires	4	f	0	1 VP	When you gain or trash this, +1 VP and gain a Silver.	t	\N	\N	\N	\N	\N	f
322	small-castle	Empires	5	f	0	Trash this or a Castle from your hand. If you do, gain a Castle.	2 VP	t	Never	Never	Never	Never	Never	t
323	haunted-castle	Empires	6	f	0	2 VP	When you gain this during your turn, gain a Gold, and each other player with 5 or more cards in hand puts 2 cards from their hand onto their deck.	t	\N	\N	\N	\N	\N	f
324	opulent-castle	Empires	7	f	0	Discard any number of Victory cards. +2 Coins per card discarded.	3 VP	t	Never	Never	Never	Never	Never	f
325	sprawling-castle	Empires	8	f	0	4 VP	When you gain this, gain a Duchy or 3 Estates.	t	\N	\N	\N	\N	\N	f
326	grand-castle	Empires	9	f	0	5 VP	When you gain this, reveal your hand. +1 VP per Victory card in your hand and/or in play.	t	\N	\N	\N	\N	\N	f
327	king's-castle	Empires	10	f	0	Worth 2 VP per Castle you have.	\N	t	\N	\N	\N	\N	\N	f
328	catapult	Empires	3	f	0	+1 Coin. Trash a card from your hand. If it costs 3 Coins or more, each other player gains a Curse. If it's a Treasure, each other player discards down to 3 cards in hand.	\N	t	Never	Never	Never	Never	Never	t
329	rocks	Empires	4	f	0	1 Coin	When you gain or trash this, gain a Silver; if it is your Buy phase, put the Silver on your deck, otherwise put it into your hand.	t	\N	\N	\N	\N	\N	f
330	chariot-race	Empires	3	f	0	+1 Action. Reveal the top card of your deck and put it into your hand. The player to your left reveals the top card of their deck. If your card costs more, +1 Coin and +1 VP.	\N	t	Always	Never	Always	Never	Never	f
331	enchantress	Empires	3	f	0	Until your next turn, the first time each other player plays an Action card on their turn, they get +1 Card and +1 Action instead of following its instructions. At the start of your next turn, +2 Cards.	\N	t	Never	Never	Never	Never	Never	f
332	farmers'-market	Empires	3	f	0	+1 Buy. If there are 4 VP or more on the Farmers' Market Supply pile, take them and trash this. Otherwise, add 1 VP to the pile and then +1 Coin per 1 VP on the pile.	\N	t	Never	Never	Never	Never	Always	f
333	gladiator	Empires	3	f	0	+2 Coins. Reveal a card from your hand. The player to your left may reveal a copy from their hand. If they do not, +1 Coin and trash a Gladiator from the Supply.	\N	t	Never	Never	Never	Never	Never	f
334	fortune	Empires	8	f	8	+1 Buy. When you play this, double your Coins if you haven't yet this turn.	\N	t	\N	\N	\N	\N	Always	f
335	sacrifice	Empires	4	f	0	Trash a card from your hand. If it's an... Action card, +2 Cards, +2 Actions. Treasure card, +2 Coins. Victory card, +2 VP.	\N	t	Sometimes	Sometimes	Sometimes	Sometimes	Never	t
336	temple	Empires	4	f	0	+1 VP. Trash from 1 to 3 differently named cards from your hand. Add 1 VP to the Temple Supply pile.	When you gain this, take the VP from the Temple Supply pile.	t	Never	Never	Never	Never	Never	t
337	villa	Empires	4	f	0	+2 Actions. +1 Buy. +1 Coin.	When you gain this, put it into your hand, +1 Action, and if it's your Buy phase return to your Action phase.	t	Always	Always	Never	Never	Always	f
338	archive	Empires	5	f	0	+1 Action. Set aside the top 3 cards of your deck face down (you may look at them). Now and at the start of your next two turns, put one into your hand.	\N	t	Always	Never	Always	Never	\N	f
339	capital	Empires	5	f	0	6 Coins. +1 Buy.	When you discard this from play, take 6 Debt, and then you may pay off Debt.	t	\N	\N	\N	\N	Always	f
340	charm	Empires	5	f	0	When you play this, choose one: +1 Buy and +2 Coins; or the next time you buy a card this turn, you may also gain a differently named card with the same cost.	\N	t	\N	\N	\N	\N	Sometimes	f
341	crown	Empires	5	f	0	If it's your Action phase, you may play an Action from your hand twice. If it's your Buy phase, you may play a Treasure from your hand twice.	\N	t	Sometimes	Sometimes	Sometimes	Sometimes	Sometimes	f
342	forum	Empires	5	f	0	+3 Cards. +1 Action. Discard 2 cards.	When you buy this, +1 Buy.	t	Always	Never	Always	Never	Never	f
343	groundskeeper	Empires	5	f	0	+1 Card. +1 Action.	While this is in play, when you gain a Victory card, +1 VP.	t	Always	Never	Always	Never	Never	f
344	legionary	Empires	5	f	0	+3 Coins. You may reveal a Gold from your hand. If you do, each other player discards down to 2 cards in hand, then draws a card.	\N	t	Never	Never	Never	Never	Never	f
345	wild-hunt	Empires	5	f	0	Choose one: +3 Cards and add 1 VP to the Wild Hunt Supply pile; or gain an Estate, and if you do, take the VP from the pile.	\N	t	Never	Never	Sometimes	Sometimes	Never	f
346	triumph	Empires	0	f	5	Gain an Estate. If you did, +1 VP per card you've gained this turn.	\N	f	\N	\N	\N	\N	\N	f
347	annex	Empires	0	f	8	Look through your discard pile. Shuffle all but up to 5 cards from it into your deck. Gain a Duchy.	\N	f	\N	\N	\N	\N	\N	f
348	donate	Empires	0	f	8	After this turn, put all cards from your deck and discard pile into your hand, trash any number, shuffle your hand into your deck, then draw 5 cards.	\N	f	\N	\N	\N	\N	\N	t
349	advance	Empires	0	f	0	You may trash an Action card from your hand. If you do, gain an Action card costing up to 6 Coins.	\N	f	\N	\N	\N	\N	\N	t
350	delve	Empires	2	f	0	+1 Buy. Gain a Silver.	\N	f	\N	\N	\N	\N	Always	f
351	tax	Empires	2	f	0	Add 2 Debt to a Supply pile.	Setup: Add 1 Debt to each Supply pile. When a player buys a card, they take the Debt from its pile.	f	\N	\N	\N	\N	\N	f
352	banquet	Empires	3	f	0	Gain 2 Coppers and a non-Victory card costing up to 5 Coins.	\N	f	\N	\N	\N	\N	\N	f
353	ritual	Empires	4	f	0	Gain a Curse. If you do, trash a card from your hand. +1 VP per 1 Coin it cost.	\N	f	\N	\N	\N	\N	\N	t
354	salt-the-earth	Empires	4	f	0	+1 VP. Trash a Victory card from the Supply.	\N	f	\N	\N	\N	\N	\N	f
355	wedding	Empires	4	f	3	+1 VP. Gain a Gold.	\N	f	\N	\N	\N	\N	\N	f
356	windfall	Empires	5	f	0	If your deck and discard pile are empty, gain 3 Golds.	\N	f	\N	\N	\N	\N	\N	f
357	conquest	Empires	6	f	0	Gain 2 Silvers. +1 VP per Silver you've gained this turn.	\N	f	\N	\N	\N	\N	\N	f
358	dominate	Empires	14	f	0	Gain a Province. If you do, +9 VP.	\N	f	\N	\N	\N	\N	\N	f
359	aqueduct	Empires	\N	\N	\N	When you gain a Treasure, move 1 VP from its pile to this. When you gain a Victory card, take the VP from this.	Setup: Put 8 VP on the Silver and Gold piles.	f	\N	\N	\N	\N	\N	f
360	arena	Empires	\N	\N	\N	At the start of your Buy phase, you may discard an Action card. If you do, take 2 VP from here.	Setup: Put 6 VP here per player.	f	\N	\N	\N	\N	\N	f
361	bandit-fort	Empires	\N	\N	\N	When scoring, -2 VP for each Silver and each Gold you have.	\N	f	\N	\N	\N	\N	\N	f
362	basilica	Empires	\N	\N	\N	When you buy a card, if you have 2 Coins or more left, take 2 VP from here.	Setup: Put 6 VP here per player.	f	\N	\N	\N	\N	\N	f
363	baths	Empires	\N	\N	\N	When you end your turn without having gained a card, take 2 VP from here.	Setup: Put 6 VP here per player.	f	\N	\N	\N	\N	\N	f
364	battlefield	Empires	\N	\N	\N	When you gain a Victory card, take 2 VP from here.	Setup: Put 6 VP here per player.	f	\N	\N	\N	\N	\N	f
365	colonnade	Empires	\N	\N	\N	When you buy an Action card, if you have a copy of it in play, take 2 VP from here.	Setup: Put 6 VP here per player.	f	\N	\N	\N	\N	\N	f
386	monastery	Nocturne	2	f	0	For each card you've gained this turn, you may trash a card from your hand or a Copper you have in play.	\N	t	\N	\N	\N	\N	\N	t
366	fountain	Empires	\N	\N	\N	When scoring, 15 VP if you have at least 10 Coppers.	\N	f	\N	\N	\N	\N	\N	f
367	defiled-shrine	Empires	\N	\N	\N	When you gain an Action, move 1 VP from its pile to this. When you buy a Curse, take the VP from this.	Setup: Put 2 VP on each non-Gathering Action Supply pile.	f	\N	\N	\N	\N	\N	f
368	fountain	Empires	\N	\N	\N	When scoring, 15 VP if you have at least 10 Coppers.	\N	f	\N	\N	\N	\N	\N	f
369	keep	Empires	\N	\N	\N	When scoring, 5 VP per differently named Treasure you have, that you have more copies of than each other player, or tied for most.	\N	f	\N	\N	\N	\N	\N	f
370	labyrinth	Empires	\N	\N	\N	When you gain a 2nd card in one of your turns, take 2 VP from here.	Setup: Put 6 VP here per player.	f	\N	\N	\N	\N	\N	f
371	mountain-pass	Empires	\N	\N	\N	When you are the first player to gain a Province, after that turn, each player bids once, up to 40 Debt, ending with you. High bidder gets +8 VP and takes the Debt they bid.	\N	f	\N	\N	\N	\N	\N	f
372	museum	Empires	\N	\N	\N	When scoring, 2 VP per differently named card you have.	\N	f	\N	\N	\N	\N	\N	f
373	obelisk	Empires	\N	\N	\N	When scoring, 2 VP per card you have from the chosen pile.	Setup: Choose a random Action Supply pile.	f	\N	\N	\N	\N	\N	f
374	orchard	Empires	\N	\N	\N	When scoring, 4 VP per differently named Action card you have 3 or more copies of.	\N	f	\N	\N	\N	\N	\N	f
375	palace	Empires	\N	\N	\N	When scoring, 3 VP per set you have of Copper - Silver - Gold.	\N	f	\N	\N	\N	\N	\N	f
376	tomb	Empires	\N	\N	\N	When you trash a card, +1 VP.	\N	f	\N	\N	\N	\N	\N	f
377	tower	Empires	\N	\N	\N	When scoring, 1 VP per non-Victory card you have from an empty Supply pile.	\N	f	\N	\N	\N	\N	\N	f
378	triumphal-arch	Empires	\N	\N	\N	When scoring, 3 VP per copy you have of the 2nd most common Action card among your cards (if it's a tie, count either).	\N	f	\N	\N	\N	\N	\N	f
379	wall	Empires	\N	\N	\N	When scoring, -1 VP per card you have after the first 15.	\N	f	\N	\N	\N	\N	\N	f
380	wolf-den	Empires	\N	\N	\N	When scoring, -3 VP per card you have exactly one copy of.	\N	f	\N	\N	\N	\N	\N	f
381	will-o'-wisp	Nocturne	0	f	0	+1 Card. +1 Action. Reveal the top card of your deck. If it costs 2 Coins or less, put it into your hand. (This is not in the Supply.)	\N	f	Always	Never	Always	Sometimes	Never	f
382	wish	Nocturne	0	f	0	+1 Action. Return this to its pile. If you did, gain a card to your hand costing up to 6 Coins. (This is not in the Supply.)	\N	f	Always	Never	Sometimes	Never	Never	f
383	druid	Nocturne	2	f	0	+1 Buy. Receive one of the set-aside Boons (leaving it there).	Setup: Set aside the top 3 Boons face up.	t	Sometimes	Never	Sometimes	Never	Always	f
384	faithful-hound	Nocturne	2	f	0	+2 Cards	When you discard this other than during Clean-up, you may set it aside, and put it into your hand at end of turn.	t	Never	Never	Always	Always	Never	f
385	guardian	Nocturne	2	f	0	Until your next turn, when another player plays an Attack card, it doesn't affect you. At the start of your next turn, +1 Coin.	This is gained to your hand (instead of your discard pile).	t	\N	\N	\N	\N	\N	f
387	pixie	Nocturne	2	f	0	+1 Card. +1 Action. Discard the top Boon. You may trash this to receive that Boon twice.	\N	t	Always	Sometimes	Always	Sometimes	Sometimes	f
388	goat	Nocturne	2	f	0	1 Coin. When you play this, you may trash a card from your hand	\N	f	\N	\N	\N	\N	\N	t
389	tracker	Nocturne	2	f	0	+1 Coin. Receive a Boon.	While this is in play, when you gain a card, you may put that card onto your deck.	t	Sometimes	Never	Sometimes	Never	Sometimes	f
390	pouch	Nocturne	2	f	0	1 Coin. +1 Buy.	\N	f	\N	\N	\N	\N	Always	f
391	imp	Nocturne	2	f	0	+2 Cards. You may play an Action card from your hand that you don't have a copy of in play. (This is not in the Supply.)	\N	f	Sometimes	Sometimes	Always	Always	Sometimes	f
392	changeling	Nocturne	3	f	0	Trash this. Gain a copy of a card you have in play.	In games using this, when you gain a card costing 3 Coins or more, you may exchange it for a Changeling.	t	\N	\N	\N	\N	\N	f
393	fool	Nocturne	3	f	0	If you aren't the player with Lost in the Woods, take it, take 3 Boons, and receive the Boons in any order.	\N	t	Sometimes	Never	Sometimes	Never	Sometimes	f
394	lost-in-the-woods	Nocturne	\N	\N	\N	At the start of your turn, you may discard a card to receive a Boon.	\N	f	\N	\N	\N	\N	\N	f
395	lucky-coin	Nocturne	4	f	0	1 Coin. When you play this, gain a Silver.	\N	f	\N	\N	\N	\N	\N	f
396	ghost-town	Nocturne	3	f	0	At the start of your next turn, +1 Card and +1 Action.	This is gained to your hand (instead of your discard pile).	t	\N	\N	\N	\N	\N	f
397	leprechaun	Nocturne	3	f	0	Gain a Gold. If you have exactly 7 cards in play, gain a Wish from its pile. Otherwise, receive a Hex.	\N	t	Never	Never	Sometimes	Never	Never	f
398	night-watchman	Nocturne	3	f	0	Look at the top 5 cards of your deck, discard any number, and put the rest back in any order.	This is gained to your hand (instead of your discard pile).	t	\N	\N	\N	\N	\N	f
399	secret-cave	Nocturne	3	f	0	+1 Card. +1 Action. You may discard 3 cards. If you did, then at the start of your next turn, +3 Coins.	\N	t	Always	Never	Sometimes	Never	Never	f
400	magic-lamp	Nocturne	0	f	0	1 Coin. When you play this, if there are at least 6 cards that you have exactly 1 copy of in play, trash this. If you do, gain 3 Wishes from their pile.	\N	f	\N	\N	\N	\N	\N	f
401	bard	Nocturne	4	f	0	+2 Coins. Receive a Boon.	\N	t	Sometimes	Never	Sometimes	Never	Sometimes	f
402	blessed-village	Nocturne	4	f	0	+1 Card. +2 Actions.	When you gain this, take a Boon. Receive it now or at the start of your next turn.	t	Always	Always	Always	Never	Never	f
403	cemetery	Nocturne	4	f	0	2 VP	When you gain this, trash up to 4 cards from your hand.	t	\N	\N	\N	\N	\N	t
404	haunted-mirror	Nocturne	0	f	0	1 Coin	When you trash this, you may discard an Action card, to gain a Ghost from its pile.	f	\N	\N	\N	\N	\N	f
405	conclave	Nocturne	4	f	0	+2 Coins. You may play an Action card from your hand that you don't have a copy of in play. If you do, +1 Action.	\N	t	Sometimes	Sometimes	Sometimes	Sometimes	Sometimes	f
406	devil's-workshop	Nocturne	4	f	0	If the number of cards you've gained this turn is: 2+, gain an Imp from its pile; 1, gain a card costing up to 4 Coins; 0, gain a Gold.	\N	t	\N	\N	\N	\N	\N	f
407	exorcist	Nocturne	4	f	0	Trash a card from your hand. Gain a cheaper Spirit from one of the Spirit piles.	\N	t	\N	\N	\N	\N	\N	t
409	zombie-apprentice	Nocturne	3	f	0	You may trash an Action card from your hand for +3 Cards and +1 Action.	\N	f	Sometimes	Never	Sometimes	Sometimes	Never	t
421	pooka	Nocturne	5	f	0	You may trash a Treasure other than Cursed Gold from your hand, for +4 Cards.	\N	t	Never	Never	Sometimes	Sometimes	Sometimes	t
410	zombie-mason	Nocturne	3	f	0	Trash the top card of your deck. You may gain a card costing up to 1 Coin more than it.	\N	f	Never	Never	Never	Never	Never	t
408	necromancer	Nocturne	4	f	0	Play a face up, non-Duration Action card from the trash, leaving it there and turning it face down for the turn.	Setup: Put the 3 Zombies into the trash.	t	Sometimes	Sometimes	Sometimes	Sometimes	Sometimes	f
411	zombie-spy	Nocturne	3	f	0	+1 Card. +1 Action. Look at the top card of your deck. Discard it or put it back.	\N	f	Always	Never	Always	Never	Never	f
412	shepherd	Nocturne	4	f	0	+1 Action. Discard any number of Victory cards, revealing them. +2 Cards per card discarded.	\N	t	Always	Never	Sometimes	Sometimes	Never	f
413	pasture	Nocturne	2	f	0	1 Coin	Worth 1 VP per Estate you have.	f	\N	\N	\N	\N	\N	f
414	skulk	Nocturne	4	f	0	+1 Buy. Each other player receives the next Hex.	When you gain this, gain a Gold.	t	Never	Never	Never	Never	Always	f
415	ghost	Nocturne	4	f	0	Reveal cards from your deck until you reveal an Action. Discard the other cards and set aside the Action. At the start of your next turn, play it twice. (This is not in the Supply.)	\N	f	\N	\N	\N	\N	\N	f
416	cobbler	Nocturne	5	f	0	At the start of your next turn, gain a card to your hand costing up to 4 Coins.	\N	t	\N	\N	\N	\N	\N	f
417	crypt	Nocturne	5	f	0	Set aside any number of Treasures you have in play, face down (under this). While any remain, at the start of each of your turns, put one of them into your hand.	\N	t	\N	\N	\N	\N	\N	f
418	cursed-village	Nocturne	5	f	0	+2 Actions. Draw until you have 6 cards in hand.	When you gain this, receive a Hex.	t	Always	Always	Sometimes	Sometimes	Never	f
419	den-of-sin	Nocturne	5	f	0	At the start of your next turn, +2 Cards.	This is gained to your hand (instead of your discard pile).	t	\N	\N	\N	\N	\N	f
420	idol	Nocturne	5	f	0	2 Coins. When you play this, if you then have an odd number of Idols in play, receive a Boon; if an even number, each other player gains a Curse.	\N	t	\N	\N	Sometimes	\N	Sometimes	f
422	cursed-gold	Nocturne	4	f	0	3 Coins. When you play this, gain a Curse.	\N	f	\N	\N	\N	\N	\N	f
423	sacred-grove	Nocturne	5	f	0	+1 Buy. +3 Coins. Receive a Boon. If it doesn't give +1 Coin, each other player may receive it.	\N	t	Sometimes	Never	Sometimes	Never	Always	f
424	tormentor	Nocturne	5	f	0	+2 Coins. If you have no other cards in play, gain an Imp from its pile. Otherwise, each other player receives the next Hex.	\N	t	Never	Never	Never	Never	Never	f
426	vampire	Nocturne	5	f	0	Each other player receives the next Hex. Gain a card costing up to 5 Coins other than a Vampire. Exchange this for a Bat.	\N	t	\N	\N	\N	\N	\N	f
425	tragic-hero	Nocturne	5	f	0	+3 Cards. +1 Buy. If you have 8 or more cards in hand (after drawing), trash this and gain a Treasure.	\N	t	Never	Never	Always	Always	Always	f
427	bat	Nocturne	2	f	0	Trash up to 2 cards from your hand. If you trashed at least one, exchange this for a Vampire. (This is not in the Supply.)	\N	f	\N	\N	\N	\N	\N	t
428	werewolf	Nocturne	5	f	0	If it's your Night phase, each other player receives the next Hex. Otherwise, +3 Cards.	\N	t	Never	Never	Sometimes	Sometimes	Never	f
429	raider	Nocturne	6	f	0	Each other player with 5 or more cards in hand discards a copy of a card you have in play (or reveals they can't). At the start of your next turn, +3 Coins.	\N	t	\N	\N	\N	\N	\N	f
430	the-earth's-gift	Nocturne	\N	\N	\N	You may discard a Treasure to gain a card costing up to 4 Coins.	\N	f	\N	\N	\N	\N	\N	f
431	the-field's-gift	Nocturne	\N	\N	\N	+1 Action. +1 Coin. (Keep this until Clean-up.)	\N	f	\N	\N	\N	\N	\N	f
432	the-flame's-gift	Nocturne	\N	\N	\N	You may trash a card from your hand.	\N	f	\N	\N	\N	\N	\N	t
433	the-forest's-gift	Nocturne	\N	\N	\N	+1 Buy. +1 Coin. (Keep this until Clean-up.)	\N	f	\N	\N	\N	\N	Always	f
434	the-moon's-gift	Nocturne	\N	\N	\N	Look through your discard pile. You may put a card from it onto your deck.	\N	f	\N	\N	\N	\N	\N	f
435	the-mountain's-gift	Nocturne	\N	\N	\N	Gain a Silver.	\N	f	\N	\N	\N	\N	\N	f
436	the-river's-gift	Nocturne	\N	\N	\N	+1 Card at the end of this turn. (Keep this until Clean-up.)	\N	f	\N	\N	\N	\N	\N	f
437	the-sea's-gift	Nocturne	\N	\N	\N	+1 Card	\N	f	\N	\N	\N	\N	\N	f
438	the-sky's-gift	Nocturne	\N	\N	\N	You may discard 3 cards to gain a Gold.	\N	f	\N	\N	\N	\N	\N	f
439	the-sun's-gift	Nocturne	\N	\N	\N	Look at the top 4 cards of your deck. Discard any number of them and put the rest back in any order.	\N	f	\N	\N	\N	\N	\N	f
440	the-swamp's-gift	Nocturne	\N	\N	\N	Gain a Will-o'-Wisp from its pile.	\N	f	\N	\N	\N	\N	\N	f
441	the-wind's-gift	Nocturne	\N	\N	\N	+2 Cards. Discard 2 cards.	\N	f	\N	\N	\N	\N	\N	f
442	bad-omens	Nocturne	\N	\N	\N	Put your deck into your discard pile. Look through it and put 2 Coppers from it onto your deck (or reveal you can't).	\N	f	\N	\N	\N	\N	\N	f
443	delusion	Nocturne	\N	\N	\N	If you don't have Deluded or Envious, take Deluded.	\N	f	\N	\N	\N	\N	\N	f
444	deluded	Nocturne	\N	\N	\N	At the start of your Buy phase, return this, and you can't buy Actions this turn.	\N	f	\N	\N	\N	\N	\N	f
445	envy	Nocturne	\N	\N	\N	If you don't have Deluded or Envious, take Envious.	\N	f	\N	\N	\N	\N	\N	f
446	envious	Nocturne	\N	\N	\N	At the start of your Buy phase, return this, and Silver and Gold make 1 Coin this turn.	\N	f	\N	\N	\N	\N	\N	f
447	famine	Nocturne	\N	\N	\N	Reveal the top 3 cards of your deck. Discard the Actions. Shuffle the rest into your deck.	\N	f	\N	\N	\N	\N	\N	f
448	fear	Nocturne	\N	\N	\N	If you have at least 5 cards in hand, discard an Action or Treasure (or reveal you can't).	\N	f	\N	\N	\N	\N	\N	f
449	greed	Nocturne	\N	\N	\N	Gain a Copper onto your deck.	\N	f	\N	\N	\N	\N	\N	f
450	haunting	Nocturne	\N	\N	\N	If you have at least 4 cards in hand, put one of them onto your deck.	\N	f	\N	\N	\N	\N	\N	f
451	locusts	Nocturne	\N	\N	\N	Trash the top card of your deck. If it's Copper or Estate, gain a Curse. Otherwise, gain a cheaper card that shares a type with it.	\N	f	\N	\N	\N	\N	\N	t
452	misery	Nocturne	\N	\N	\N	If this is your first Misery this game, take Miserable. Otherwise, flip it over to Twice Miserable.	\N	f	\N	\N	\N	\N	\N	f
453	miserable	Nocturne	\N	\N	\N	-2 VP	\N	f	\N	\N	\N	\N	\N	f
454	twice-miserable	Nocturne	\N	\N	\N	-4 VP	\N	f	\N	\N	\N	\N	\N	f
455	plague	Nocturne	\N	\N	\N	Gain a Curse to your hand.	\N	f	\N	\N	\N	\N	\N	f
456	poverty	Nocturne	\N	\N	\N	Discard down to 3 cards in hand.	\N	f	\N	\N	\N	\N	\N	f
457	war	Nocturne	\N	\N	\N	Reveal cards from your deck until revealing one costing 3 Coins or 4 Coins. Trash it and discard the rest.	\N	f	\N	\N	\N	\N	\N	f
459	horn	Renaissance	\N	\N	\N	Once per turn, when you discard a Border Guard from play, you may put it onto your deck.	\N	f	\N	\N	\N	\N	\N	f
472	patron	Renaissance	4	f	0	+1 Villager. +2 Coins.	When something causes you to reveal this (using the word "reveal"), +1 Coffers.	t	Always	Never	Never	Never	Never	f
460	lantern	Renaissance	\N	\N	\N	Your Border Guards reveal 3 cards and discard 2. (It takes all 3 being Actions to take the Horn.)	\N	f	\N	\N	\N	\N	\N	f
458	border-guard	Renaissance	2	f	0	+1 Action. Reveal the top 2 cards of your deck. Put one into your hand and discard the other. If both were Actions, take the Lantern or Horn.	\N	t	Always	Never	Always	Never	Never	f
461	ducat	Renaissance	2	f	0	+1 Coffers. +1 Buy.	When you gain this, you may trash a Copper from your hand.	t	\N	\N	\N	\N	Always	t
462	lackeys	Renaissance	2	f	0	+2 Cards	When you gain this, +2 Villagers.	t	Never	Never	Always	Always	Never	f
463	acting-troupe	Renaissance	3	f	0	+4 Villagers. Trash this.	\N	t	Always	Always	Never	Never	Never	f
464	cargo-ship	Renaissance	3	f	0	+2 Coins. Once this turn, when you gain a card, you may set it aside face up (on this). At the start of your next turn, put it into your hand.	\N	t	Never	Never	Never	Never	Never	f
465	experiment	Renaissance	3	f	0	+2 Cards. +1 Action. Return this to the Supply.	When you gain this, gain another Experiment (that doesn't come with another).	t	Always	Never	Always	Always	Never	f
466	improve	Renaissance	3	f	0	+2 Coins. At the start of Clean-up, you may trash an Action card you would discard from play this turn, to gain a card costing exactly 1 Coin more than it.	\N	t	Never	Never	Never	Never	Never	t
467	flag-bearer	Renaissance	4	f	0	+2 Coins	When you gain or trash this, take the Flag.	t	Never	Never	Never	Never	Never	f
468	flag	Renaissance	\N	\N	\N	When drawing your hand, +1 Card.	\N	f	\N	\N	\N	\N	\N	f
469	hideout	Renaissance	4	f	0	+1 Card. +2 Actions. Trash a card from your hand. If it's a Victory card, gain a Curse.	\N	t	Always	Always	Always	Never	Never	t
470	inventor	Renaissance	4	f	0	Gain a card costing up to 4 Coins, then cards cost 1 Coin less this turn (but not less than 0 Coins).	\N	t	Never	Never	Never	Never	Never	f
471	mountain-village	Renaissance	4	f	0	+2 Actions. Look through your discard pile and put a card from it into your hand; if you can't, +1 Card.	\N	t	Always	Always	Always	Never	Never	f
473	priest	Renaissance	4	f	0	+2 Coins. Trash a card from your hand. For the rest of this turn, when you trash a card, +2 Coins.	\N	t	Never	Never	Never	Never	Never	t
474	research	Renaissance	4	f	0	+1 Action. Trash a card from your hand. Per 1 Coin it costs, set aside a card from your deck face down (on this). At the start of your next turn, put those cards into your hand.	\N	t	Always	Never	Never	Never	Never	t
475	silk-merchant	Renaissance	4	f	0	+2 Cards. +1 Buy.	When you gain or trash this, +1 Coffers and +1 Villager	t	Never	Never	Never	Never	Always	f
476	old-witch	Renaissance	5	f	0	+3 Cards. Each other player gains a Curse and may trash a Curse from their hand.	\N	t	Never	Never	Always	Always	Never	f
477	recruiter	Renaissance	5	f	0	+2 Cards. Trash a card from your hand. +1 Villager per 1 Coin it costs.	\N	t	Sometimes	Sometimes	Always	Always	Never	t
478	scepter	Renaissance	5	f	0	When you play this, choose one: +2 Coins; or replay an Action card you played this turn that's still in play.	\N	t	\N	\N	Sometimes	Sometimes	Sometimes	f
479	scholar	Renaissance	5	f	0	Discard your hand. +7 Cards.	\N	t	Never	Never	Sometimes	Sometimes	Never	f
480	sculptor	Renaissance	5	f	0	Gain a card to your hand costing up to 4 Coins. If it's a Treasure, +1 Villager.	\N	t	Sometimes	Never	Always	Never	Never	f
481	seer	Renaissance	5	f	0	+1 Card. +1 Action. Reveal the top 3 cards of your deck. Put the ones costing from 2 Coins to 4 Coins into your hand. Put the rest back in any order.	\N	t	Always	Never	Always	Sometimes	Never	f
482	spices	Renaissance	5	f	0	2 Coins. +1 Buy.	When you gain this, +2 Coffers.	t	\N	\N	\N	\N	Always	f
483	swashbuckler	Renaissance	5	f	0	+3 Cards. If your discard pile has any card in it: +1 Coffers, then if you have at least 4 Coffers tokens, take the Treasure Chest.	\N	t	Never	Never	Always	Always	Never	f
484	treasure-chest	Renaissance	\N	\N	\N	At the start of your Buy phase, gain a Gold.	\N	f	\N	\N	\N	\N	\N	f
485	treasurer	Renaissance	5	f	0	+3 Coins. Choose one: Trash a Treasure from your hand; or gain a Treasure from the trash to your hand; or take the Key.	\N	t	Never	Never	Sometimes	Never	Never	t
486	key	Renaissance	\N	\N	\N	At the start of your turn, +1 Coin.	\N	f	\N	\N	\N	\N	\N	f
487	villain	Renaissance	5	f	0	+2 Coffers. Each other player with 5 or more cards in hand discards one costing 2 Coins or more (or reveals they can't).	\N	t	Never	Never	Never	Never	Never	f
489	city-gate	Renaissance	3	f	0	At the start of your turn, +1 Card, then put a card from your hand onto your deck.	\N	f	\N	\N	\N	\N	\N	f
488	cathedral	Renaissance	3	f	0	At the start of your turn, trash a card from your hand.	\N	f	\N	\N	\N	\N	\N	t
490	pageant	Renaissance	3	f	0	At the end of your Buy phase, you may pay 1 Coin for +1 Coffers.	\N	f	\N	\N	\N	\N	\N	f
491	sewers	Renaissance	3	f	0	When you trash a card other than with this, you may trash a card from your hand.	\N	f	\N	\N	\N	\N	\N	t
492	star-chart	Renaissance	3	f	0	When you shuffle, you may pick one of the cards to go on top.	\N	f	\N	\N	\N	\N	\N	f
493	exploration	Renaissance	4	f	0	At the end of your Buy phase, if you didn't buy any cards, +1 Coffers and +1 Villager.	\N	f	\N	\N	\N	\N	\N	f
494	fair	Renaissance	4	f	0	At the start of your turn, +1 Buy.	\N	f	\N	\N	\N	\N	Always	f
495	silos	Renaissance	4	f	0	At the start of your turn, discard any number of Coppers, revealed, and draw that many cards.	\N	f	\N	\N	\N	\N	\N	f
496	sinister-plot	Renaissance	4	f	0	At the start of your turn, add a token here, or remove your tokens here for +1 Card each.	\N	f	\N	\N	\N	\N	\N	f
497	academy	Renaissance	5	f	0	When you gain an Action card, +1 Villager	\N	f	\N	\N	\N	\N	\N	f
498	capitalism	Renaissance	5	f	0	During your turns, Actions with +Coin amounts in their text are also Treasures.	\N	f	\N	\N	\N	\N	\N	f
499	fleet	Renaissance	5	f	0	After the game ends, there's an extra round of turns just for players with this.	\N	f	\N	\N	\N	\N	\N	f
500	guildhall	Renaissance	5	f	0	When you gain a Treasure, +1 Coffers.	\N	f	\N	\N	\N	\N	\N	f
501	piazza	Renaissance	5	f	0	At the start of your turn, reveal the top card of your deck. If it's an Action, play it.	\N	f	\N	\N	\N	\N	\N	f
502	road-network	Renaissance	5	f	0	When another player gains a Victory card, +1 Card.	\N	f	\N	\N	\N	\N	\N	f
503	barracks	Renaissance	6	f	0	At the start of your turn, +1 Action.	\N	f	\N	\N	\N	\N	\N	f
504	crop-rotation	Renaissance	6	f	0	At the start of your turn, you may discard a Victory card for +2 Cards.	\N	f	\N	\N	\N	\N	\N	f
505	innovation	Renaissance	6	f	0	The first time you gain an Action card in each of your turns, you may set it aside. If you do, play it.	\N	f	\N	\N	\N	\N	\N	f
506	canal	Renaissance	7	f	0	During your turns, cards cost 1 Coin less, but not less than 0 Coins.	\N	f	\N	\N	\N	\N	\N	f
507	citadel	Renaissance	8	f	0	The first time you play an Action card during each of your turns, play it again afterwards.	\N	f	\N	\N	\N	\N	\N	f
510	church	Promo	3	f	0	+1 Action. Set aside up to 3 cards from your hand face down. At the start of your next turn, put them into your hand, then you may trash a card from your hand.	\N	t	Always	Never	Never	Never	Never	t
508	church	Promo	3	f	0	+1 Action. Set aside up to 3 cards from your hand face down. At the start of your next turn, put them into your hand, then you may trash a card from your hand.	\N	t	Always	Never	Never	Never	Never	t
509	black-market	Promo	3	f	0	+2 Coins. Reveal the top 3 cards of the Black Market deck. Play any number of Treasures from your hand. You may buy one of the revealed cards. Put the rest on the bottom of the Black Market deck in any order.	Setup: Make a Black Market deck out of different unused Kingdom cards.	t	Never	\N	Never	Never	Never	f
511	dismantle	Promo	4	f	0	Trash a card from your hand. If it costs 1 Coin or more, gain a cheaper card and a Gold.	\N	t	Never	Never	Never	Never	Never	t
512	envoy	Promo	4	f	0	Reveal the top 5 cards of your deck. The player to your left chooses one. Discard that one and put the rest into your hand.	\N	t	Never	Never	Always	Always	Never	f
513	sauna	Promo	4	f	0	+1 Card. +1 Action. You may play an Avanto from your hand.	While this is in play, when you play a Silver, you may trash a card from your hand.	t	Always	Never	Always	Sometimes	Never	t
514	avanto	Promo	5	f	0	+3 Cards. You may play a Sauna from your hand.	\N	t	Sometimes	Never	Always	Always	Never	f
515	walled-village	Promo	4	f	0	+1 Card. +2 Actions.	At the start of Clean-up, if you have this and no more than one other Action card in play, you may put this onto your deck.	t	Always	Always	Always	Never	Never	f
516	governor	Promo	5	f	0	+1 Action. Choose one; you get the version in parentheses: Each player gets +1 (+3) Cards; or each player gains a Silver (Gold); or each player may trash a card from their hand and gain a card costing exactly 1 Coin (2 Coins) more.	\N	t	Always	Never	Sometimes	Sometimes	Never	t
517	stash	Promo	5	f	0	2 Coins	When shuffling this, you may look through your remaining deck, and may put this anywhere in the shuffled cards.	t	\N	\N	\N	\N	\N	f
518	captain	Promo	6	f	0	Now and at the start of your next turn: Play a non-Duration Action card from the Supply costing up to 4 Coins, leaving it there.	\N	t	Sometimes	Sometimes	Sometimes	Sometimes	Sometimes	f
519	prince	Promo	8	f	0	You may set this aside. If you do, set aside an Action card from your hand costing up to 4 Coins. At the start of each of your turns, play that Action, setting it aside again when you discard it from play. (Stop playing it if you fail to set it aside on a turn you play it.)	\N	t	Never	Never	Never	Never	Never	f
520	summon	Promo	5	f	0	Gain an Action card costing up to 4 Coins. Set it aside. If you did, then at the start of your next turn, play it.	\N	f	\N	\N	\N	\N	\N	f
\.


--
-- Name: card_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('card_id_seq', 520, true);


--
-- Data for Name: link_pairs; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY link_pairs (id, card_one, card_two) FROM stdin;
1	108	99
2	99	108
3	108	100
4	100	108
5	108	102
6	102	108
7	108	103
8	103	108
9	108	104
10	104	108
11	108	105
12	105	108
13	108	106
14	106	108
15	108	107
16	107	108
17	109	108
18	108	109
19	111	108
20	108	111
21	147	146
22	146	147
23	148	146
24	146	148
25	149	146
26	146	149
27	150	146
28	146	150
29	151	146
30	146	151
31	199	198
32	198	199
33	204	203
34	203	204
35	206	187
36	187	206
37	206	185
38	185	206
39	206	186
40	186	206
41	206	184
42	184	206
43	206	188
44	188	206
45	210	189
46	189	210
47	210	187
48	187	210
49	210	185
50	185	210
51	210	186
52	186	210
53	210	184
54	184	210
55	210	188
56	188	210
57	216	189
58	189	216
59	220	185
60	185	220
61	220	187
62	187	220
63	220	186
64	186	220
65	220	188
66	188	220
67	220	184
68	184	220
69	225	223
70	223	225
73	224	223
74	223	224
75	226	223
76	223	226
77	226	224
78	224	226
79	226	225
80	225	226
81	227	223
82	223	227
83	227	224
84	224	227
85	227	225
86	225	227
87	227	226
88	226	227
89	228	223
90	223	228
91	228	224
92	224	228
93	228	225
94	225	228
95	228	226
96	226	228
97	228	227
98	227	228
99	229	223
100	223	229
101	229	224
102	224	229
103	229	225
104	225	229
105	229	226
106	226	229
107	229	227
108	227	229
109	229	228
110	228	229
111	230	223
112	223	230
113	230	224
114	224	230
115	230	225
116	225	230
117	230	226
118	226	230
119	230	227
120	227	230
121	230	228
122	228	230
123	230	229
124	229	230
125	231	223
126	223	231
127	231	224
128	224	231
129	231	225
130	225	231
131	231	226
132	226	231
133	231	227
134	227	231
135	231	228
136	228	231
137	231	229
138	229	231
139	231	230
140	230	231
141	232	223
142	223	232
143	232	224
144	224	232
145	232	225
146	225	232
147	232	226
148	226	232
149	232	227
150	227	232
151	232	228
152	228	232
153	232	229
154	229	232
155	232	230
156	230	232
157	232	231
158	231	232
159	234	189
160	189	234
161	254	253
162	253	254
163	255	253
164	253	255
165	255	254
166	254	255
167	256	253
168	253	256
169	256	254
170	254	256
171	256	255
172	255	256
173	257	253
174	253	257
175	257	254
176	254	257
177	257	255
178	255	257
179	257	256
180	256	257
181	259	258
182	258	259
183	260	258
184	258	260
185	260	259
186	259	260
193	261	258
194	258	261
195	261	259
196	259	261
197	261	260
198	260	261
199	262	258
200	258	262
201	262	259
202	259	262
203	262	260
204	260	262
205	262	261
206	261	262
207	315	314
208	314	315
209	317	316
210	316	317
211	319	318
212	318	319
213	321	320
214	320	321
215	322	320
216	320	322
217	322	321
218	321	322
219	323	320
220	320	323
221	323	321
222	321	323
223	323	322
224	322	323
225	324	320
226	320	324
227	324	321
228	321	324
229	324	322
230	322	324
231	324	323
232	323	324
233	325	320
234	320	325
235	325	321
236	321	325
237	325	322
238	322	325
239	325	323
240	323	325
241	325	324
242	324	325
243	326	320
244	320	326
245	326	321
246	321	326
247	326	322
248	322	326
249	326	323
250	323	326
251	326	324
252	324	326
253	326	325
254	325	326
255	327	320
256	320	327
257	327	321
258	321	327
259	327	322
260	322	327
261	327	323
262	323	327
263	327	324
264	324	327
265	327	325
266	325	327
267	327	326
268	326	327
269	329	328
270	328	329
271	334	333
272	333	334
273	388	387
274	387	388
275	390	389
276	389	390
277	394	393
278	393	394
279	395	393
280	393	395
281	400	399
282	399	400
283	404	403
284	403	404
285	406	391
286	391	406
287	407	381
288	381	407
289	407	391
290	391	407
295	408	409
296	409	408
297	408	410
298	410	408
299	411	408
300	408	411
301	413	412
302	412	413
307	415	404
308	404	415
309	415	407
310	407	415
311	422	421
312	421	422
313	424	391
314	391	424
315	427	426
316	426	427
317	430	383
318	383	430
319	430	387
320	387	430
321	430	389
322	389	430
323	430	393
324	393	430
325	430	401
326	401	430
327	430	402
328	402	430
329	430	420
330	420	430
331	430	423
332	423	430
333	431	383
334	383	431
335	431	387
336	387	431
337	431	389
338	389	431
339	431	393
340	393	431
341	431	401
342	401	431
343	431	402
344	402	431
345	431	420
346	420	431
347	431	423
348	423	431
349	432	383
350	383	432
351	432	387
352	387	432
353	432	389
354	389	432
355	432	393
356	393	432
357	432	401
358	401	432
359	432	402
360	402	432
361	432	420
362	420	432
363	432	423
364	423	432
365	433	383
366	383	433
367	433	387
368	387	433
369	433	389
370	389	433
371	433	393
372	393	433
373	433	401
374	401	433
375	433	402
376	402	433
377	433	420
378	420	433
379	433	423
380	423	433
381	434	383
382	383	434
383	434	387
384	387	434
385	434	389
386	389	434
387	434	393
388	393	434
389	434	401
390	401	434
391	434	402
392	402	434
393	434	420
394	420	434
395	434	423
396	423	434
397	435	383
398	383	435
399	435	387
400	387	435
401	435	389
402	389	435
403	435	393
404	393	435
405	435	401
406	401	435
407	435	402
408	402	435
409	435	420
410	420	435
411	435	423
412	423	435
413	436	383
414	383	436
415	436	387
416	387	436
417	436	389
418	389	436
419	436	393
420	393	436
421	436	401
422	401	436
423	436	402
424	402	436
425	436	420
426	420	436
427	436	423
428	423	436
429	437	383
430	383	437
431	437	387
432	387	437
433	437	389
434	389	437
435	437	393
436	393	437
437	437	401
438	401	437
439	437	402
440	402	437
441	437	420
442	420	437
443	437	423
444	423	437
445	438	383
446	383	438
447	438	387
448	387	438
449	438	389
450	389	438
451	438	393
452	393	438
453	438	401
454	401	438
455	438	402
456	402	438
457	438	420
458	420	438
459	438	423
460	423	438
461	439	383
462	383	439
463	439	387
464	387	439
465	439	389
466	389	439
467	439	393
468	393	439
469	439	401
470	401	439
471	439	402
472	402	439
473	439	420
474	420	439
475	439	423
476	423	439
477	440	381
478	381	440
479	440	383
480	383	440
481	440	387
482	387	440
483	440	389
484	389	440
485	440	393
486	393	440
487	440	401
488	401	440
489	440	402
490	402	440
491	440	420
492	420	440
493	440	423
494	423	440
495	441	383
496	383	441
497	441	387
498	387	441
499	441	389
500	389	441
501	441	393
502	393	441
503	441	401
504	401	441
505	441	402
506	402	441
507	441	420
508	420	441
509	441	423
510	423	441
511	442	397
512	397	442
513	442	414
514	414	442
515	442	418
516	418	442
517	442	424
518	424	442
519	442	426
520	426	442
521	442	428
522	428	442
523	443	397
524	397	443
525	443	414
526	414	443
527	443	418
528	418	443
529	443	424
530	424	443
531	443	426
532	426	443
533	443	428
534	428	443
535	444	443
536	443	444
537	445	397
538	397	445
539	445	414
540	414	445
541	445	418
542	418	445
543	445	424
544	424	445
545	445	426
546	426	445
547	445	428
548	428	445
549	446	445
550	445	446
551	447	397
552	397	447
553	447	414
554	414	447
555	447	418
556	418	447
557	447	424
558	424	447
559	447	426
560	426	447
561	447	428
562	428	447
563	448	397
564	397	448
565	448	414
566	414	448
567	448	418
568	418	448
569	448	424
570	424	448
571	448	426
572	426	448
573	448	428
574	428	448
575	449	397
576	397	449
577	449	414
578	414	449
579	449	418
580	418	449
581	449	424
582	424	449
583	449	426
584	426	449
585	449	428
586	428	449
587	450	397
588	397	450
589	450	414
590	414	450
591	450	418
592	418	450
593	450	424
594	424	450
595	450	426
596	426	450
597	450	428
598	428	450
599	451	397
600	397	451
601	451	414
602	414	451
603	451	418
604	418	451
605	451	424
606	424	451
607	451	426
608	426	451
609	451	428
610	428	451
611	452	397
612	397	452
613	452	414
614	414	452
615	452	418
616	418	452
617	452	424
618	424	452
619	452	426
620	426	452
621	452	428
622	428	452
623	453	452
624	452	453
625	454	452
626	452	454
627	454	453
628	453	454
629	455	397
630	397	455
631	455	414
632	414	455
633	455	418
634	418	455
635	455	424
636	424	455
637	455	426
638	426	455
639	455	428
640	428	455
641	456	397
642	397	456
643	456	414
644	414	456
645	456	418
646	418	456
647	456	424
648	424	456
649	456	426
650	426	456
651	456	428
652	428	456
653	457	397
654	397	457
655	457	414
656	414	457
657	457	418
658	418	457
659	457	424
660	424	457
661	457	426
662	426	457
663	457	428
664	428	457
671	460	459
672	459	460
673	458	459
674	459	458
675	458	460
676	460	458
677	468	467
678	467	468
679	484	483
680	483	484
681	486	485
682	485	486
683	514	513
684	513	514
\.


--
-- Name: link_pairs_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('link_pairs_id_seq', 684, true);


--
-- Data for Name: type; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY type (id, name) FROM stdin;
1	Treasure
2	Curse
3	Victory
4	Action
5	Reaction
6	Attack
7	Duration
8	Prize
9	Ruins
10	Shelter
11	Looter
12	Knight
13	Reserve
14	Traveller
15	Event
16	Castle
17	Gathering
18	Landmark
19	Spirit
20	Fate
21	Night
22	Heirloom
23	State
24	Doom
25	Zombie
26	Boon
27	Hex
28	Artifact
29	Project
\.


--
-- Data for Name: type_card; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY type_card (id, card_id, type_id) FROM stdin;
2	1	1
3	2	2
4	3	3
5	4	1
6	5	3
7	6	1
8	7	3
9	8	4
10	9	4
11	10	4
12	10	5
14	11	4
15	12	4
16	13	4
17	14	4
18	15	4
19	16	4
20	16	6
21	17	3
22	18	4
23	18	6
24	19	4
25	20	4
27	22	4
28	23	4
29	24	4
30	24	6
33	26	4
34	25	4
35	27	4
36	28	4
37	29	4
38	30	4
39	21	4
40	31	4
41	32	4
42	32	6
43	33	4
44	34	4
45	35	4
46	36	4
47	37	4
48	37	6
49	38	4
50	38	6
51	39	4
52	40	4
53	41	4
54	42	4
55	43	4
56	44	4
57	45	4
58	46	4
59	46	6
60	47	4
61	48	4
62	49	4
63	50	4
64	51	4
65	51	5
66	52	4
67	53	4
68	53	3
70	54	4
71	55	4
72	56	4
73	57	3
74	58	4
75	58	6
76	59	4
77	60	4
78	60	6
79	61	4
80	61	6
81	62	4
82	63	4
83	64	1
84	64	3
85	65	4
86	65	3
87	66	4
88	66	5
89	67	4
90	67	3
91	68	4
92	69	4
95	71	4
96	70	4
97	70	6
98	72	4
99	73	4
100	73	7
101	74	4
102	74	7
103	75	4
106	77	4
107	78	4
108	78	6
109	79	4
110	79	7
111	80	4
112	81	4
113	82	4
115	84	4
116	84	6
117	85	4
118	85	3
119	86	4
120	87	4
121	87	6
122	88	4
123	89	4
124	89	6
125	90	4
126	91	4
128	92	4
129	93	4
130	93	6
131	94	4
132	94	7
133	95	4
134	95	7
135	96	4
136	96	7
137	97	4
138	98	4
139	98	7
140	83	4
141	83	7
142	99	4
143	100	3
144	101	4
146	102	4
147	103	4
148	103	6
149	104	4
150	105	4
151	106	4
152	106	6
153	107	1
154	108	1
155	109	4
156	110	4
157	111	4
158	112	1
159	113	4
160	114	4
161	114	5
162	115	4
163	116	4
164	117	1
165	118	1
167	120	4
168	121	1
169	119	4
170	122	4
171	123	4
172	124	4
173	124	6
174	125	4
175	125	6
176	126	1
177	127	4
178	128	1
183	130	1
184	129	4
185	131	4
186	132	4
187	132	6
188	133	1
189	134	4
190	135	4
192	136	4
193	137	4
194	138	1
195	139	3
196	140	4
197	141	4
198	141	6
199	142	4
200	143	4
201	144	4
202	144	5
203	145	4
204	146	4
209	147	4
210	147	8
211	148	1
212	148	8
213	149	4
214	149	6
215	149	8
216	150	4
217	150	8
218	151	4
219	151	8
220	152	4
221	152	6
222	153	4
223	154	1
224	155	4
225	156	4
226	156	6
227	157	3
228	158	4
229	159	4
230	160	1
231	160	5
232	161	4
233	162	4
234	163	4
235	163	6
236	164	4
237	165	3
238	165	5
240	167	4
241	167	6
242	166	4
243	168	4
244	169	3
245	170	4
246	171	1
247	172	4
248	172	5
249	173	4
250	174	4
251	175	4
252	176	4
253	177	1
254	178	4
255	179	4
256	180	4
257	180	6
258	181	4
259	182	4
260	183	3
261	184	4
262	184	9
263	185	4
264	185	9
265	186	4
266	186	9
267	187	4
268	187	9
269	188	4
270	188	9
271	189	1
272	190	4
273	191	5
274	191	10
275	192	4
276	192	10
277	193	3
278	193	10
279	194	4
280	194	5
281	195	4
282	196	4
283	197	4
284	198	4
285	199	4
286	200	4
287	200	5
288	201	4
289	202	4
290	203	4
291	203	6
292	204	4
293	204	6
294	205	4
295	206	4
296	206	11
297	207	3
298	208	4
299	209	4
300	210	4
301	210	6
302	210	11
303	211	4
304	212	4
305	213	4
306	214	4
307	215	4
308	216	4
309	217	4
310	218	4
311	219	1
312	220	4
313	220	6
314	220	11
315	221	4
316	222	4
317	223	4
318	223	6
319	223	12
324	225	4
325	225	6
326	225	12
327	224	4
328	224	6
329	224	12
330	224	3
331	226	4
332	226	6
333	226	12
334	227	4
335	227	6
336	227	12
337	228	4
338	228	6
339	228	12
340	229	4
341	229	6
342	229	12
343	230	4
344	230	6
345	230	12
346	231	4
347	231	6
348	231	12
349	232	4
350	232	6
351	232	12
352	233	4
353	234	4
354	234	6
355	235	4
356	236	4
357	236	6
358	237	4
359	238	4
361	240	4
362	239	4
363	241	4
364	242	1
365	243	4
366	244	4
367	245	4
368	245	6
369	246	4
370	247	4
371	248	4
372	249	4
373	250	4
374	251	4
375	251	6
376	252	1
377	252	13
378	253	4
379	253	14
380	254	4
381	254	14
382	255	4
383	255	6
384	255	14
385	256	4
386	256	14
387	257	4
388	257	7
389	258	4
390	258	14
391	259	4
392	259	6
393	259	14
394	260	4
395	260	14
398	261	4
399	261	14
400	262	4
401	262	13
402	263	4
403	263	13
404	264	4
405	265	4
406	265	7
407	266	4
408	266	7
409	266	5
410	267	4
411	267	7
412	268	4
413	268	7
414	269	4
415	269	13
416	270	4
417	270	13
418	271	4
419	272	4
420	273	4
421	274	4
422	275	4
423	276	4
424	276	13
425	277	4
426	278	4
427	278	6
428	278	7
429	279	4
430	279	13
431	279	3
432	280	4
433	280	6
434	281	4
435	281	6
436	281	7
437	282	4
438	283	1
439	283	6
440	284	4
441	284	13
442	285	4
443	286	4
444	286	6
445	286	7
446	287	1
447	288	4
448	288	13
449	289	4
450	289	7
451	290	15
452	291	15
453	292	15
454	293	15
455	294	15
456	295	15
457	296	15
458	297	15
459	298	15
460	299	15
461	300	15
462	301	15
463	302	15
464	303	15
465	304	15
466	305	15
467	306	15
468	307	15
469	308	15
470	309	15
471	310	4
472	311	4
473	312	4
474	313	4
475	314	4
476	315	1
477	316	4
478	317	4
479	318	4
480	319	4
481	320	1
482	320	3
483	320	16
484	321	3
485	321	16
486	322	4
487	322	3
488	322	16
489	323	3
490	323	16
491	324	4
492	324	3
493	324	16
494	325	3
495	325	16
496	326	3
497	326	16
498	327	3
499	327	16
500	328	4
501	328	6
502	329	1
503	330	4
504	331	4
505	331	6
506	331	7
507	332	4
508	332	17
509	333	4
510	334	1
511	335	4
512	336	4
513	336	17
514	337	4
515	338	4
516	338	7
517	339	1
518	340	1
519	341	4
520	341	1
521	342	4
522	343	4
523	344	4
524	344	6
525	345	4
526	345	17
527	346	15
528	347	15
529	348	15
530	349	15
531	350	15
532	351	15
533	352	15
534	353	15
535	354	15
536	355	15
537	356	15
538	357	15
539	358	15
540	359	18
541	360	18
542	361	18
543	362	18
544	363	18
545	364	18
546	365	18
549	366	18
550	367	18
551	368	18
552	369	18
553	370	18
554	371	18
555	372	18
556	373	18
557	374	18
558	375	18
559	376	18
560	377	18
561	378	18
562	379	18
563	380	18
564	381	4
565	381	19
566	382	4
567	383	4
568	383	20
569	384	4
570	384	5
571	385	21
572	385	7
573	386	21
574	387	4
575	387	20
576	388	1
577	388	22
578	389	4
579	389	20
580	390	1
581	390	22
582	391	4
583	391	19
584	392	21
585	393	4
586	393	20
587	394	23
588	395	1
589	395	22
590	396	21
591	396	7
592	397	4
593	397	24
594	398	21
595	399	4
596	399	7
597	400	1
598	400	22
599	401	4
600	401	20
601	402	4
602	402	20
603	403	3
606	404	1
607	404	22
608	405	4
609	406	21
610	407	21
612	409	4
613	409	25
615	410	4
616	410	25
617	408	4
618	411	4
619	411	25
620	412	4
621	413	1
622	413	3
623	413	22
624	414	4
625	414	6
626	414	24
630	415	21
631	415	7
632	415	19
633	416	21
634	416	7
635	417	21
636	417	7
637	418	4
638	418	24
639	419	21
640	419	7
641	420	1
642	420	6
643	420	20
644	421	4
645	422	1
646	422	22
647	423	4
648	423	20
649	424	4
650	424	6
651	424	24
653	426	21
654	426	6
655	426	24
656	425	4
657	427	21
662	428	4
663	428	21
664	428	6
665	428	24
666	429	21
667	429	7
668	429	6
669	430	26
670	431	26
671	432	26
672	433	26
673	434	26
674	435	26
675	436	26
676	437	26
677	438	26
678	439	26
679	440	26
680	441	26
681	442	27
682	443	27
683	444	23
684	445	27
685	446	23
686	447	27
687	448	27
688	449	27
689	450	27
690	451	27
691	452	27
692	453	23
693	454	23
694	455	27
695	456	27
696	457	27
698	459	28
700	460	28
701	458	4
702	461	1
703	462	4
704	463	4
705	464	4
706	464	7
707	465	4
708	466	4
709	467	4
710	468	28
711	469	4
712	470	4
713	471	4
714	472	4
715	472	5
716	473	4
717	474	4
718	474	7
719	475	4
720	476	4
721	476	6
722	477	4
723	478	1
724	479	4
726	480	4
727	481	4
728	482	1
729	483	4
730	484	28
731	485	4
732	486	28
733	487	4
734	487	6
736	489	29
737	488	29
738	490	29
739	491	29
740	492	29
741	493	29
742	494	29
743	495	29
744	496	29
745	497	29
746	498	29
747	499	29
748	500	29
749	501	29
750	502	29
751	503	29
752	504	29
753	505	29
754	506	29
755	507	29
758	508	4
759	508	7
760	509	4
761	510	4
762	510	7
763	511	4
764	512	4
765	513	4
766	514	4
767	515	4
768	516	4
769	517	1
770	518	4
771	518	7
772	519	4
773	520	15
\.


--
-- Name: type_card_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('type_card_id_seq', 773, true);


--
-- Name: type_id_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('type_id_seq', 29, true);


--
-- Name: card card_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY card
    ADD CONSTRAINT card_pkey PRIMARY KEY (id);


--
-- Name: link_pairs link_pairs_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY link_pairs
    ADD CONSTRAINT link_pairs_pkey PRIMARY KEY (id);


--
-- Name: type_card type_card_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY type_card
    ADD CONSTRAINT type_card_pkey PRIMARY KEY (id);


--
-- Name: type type_pkey; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY type
    ADD CONSTRAINT type_pkey PRIMARY KEY (id);


--
-- Name: link_pairs unique_link_pairs; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY link_pairs
    ADD CONSTRAINT unique_link_pairs UNIQUE (card_one, card_two);


--
-- Name: type_card unique_type_card; Type: CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY type_card
    ADD CONSTRAINT unique_type_card UNIQUE (card_id, type_id);


--
-- Name: link_pairs link_pairs_card_one_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY link_pairs
    ADD CONSTRAINT link_pairs_card_one_fkey FOREIGN KEY (card_one) REFERENCES card(id);


--
-- Name: link_pairs link_pairs_card_two_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY link_pairs
    ADD CONSTRAINT link_pairs_card_two_fkey FOREIGN KEY (card_two) REFERENCES card(id);


--
-- Name: type_card type_card_card_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY type_card
    ADD CONSTRAINT type_card_card_id_fkey FOREIGN KEY (card_id) REFERENCES card(id);


--
-- Name: type_card type_card_type_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: postgres
--

ALTER TABLE ONLY type_card
    ADD CONSTRAINT type_card_type_id_fkey FOREIGN KEY (type_id) REFERENCES type(id);


--
-- PostgreSQL database dump complete
--


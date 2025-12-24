****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　セキチュー支払照合　　　　　　　　*
*    モジュール名　　　　：　支払明細表作成　植物　　　　　　　*
*    作成日／更新日　　　：　10/04/21                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　支払情報ファイルより支払明細表を　*
*                        ：　印刷する。　　　　　　　　　　　　*
*    作成日／更新日　　　：　14/04/28                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　消費税率を出力する。　　　　　　　*
*    作成日／更新日　　　：　16/04/27                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　伝票区分追加　９１、９２　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI1510L.
 AUTHOR.                OONO.
 DATE-WRITTEN.          10/04/21.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         YA        IS   PITCH-2
         YB        IS   PITCH-15
         YB-21     IS   BAIKAKU-15
         YA-21     IS   BAIKAKU
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 支払情報データ >>--*
     SELECT   SSIHARAD  ASSIGN         DA-01-S-SSIHARAD
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SSI-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         TOK-ST.
*----<< 店舗マスタ >>--*
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52   TEN-F011
                        STATUS         TEN-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN         LP-04.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 支払合計ファイル >>--*
 FD  SSIHARAD
                        BLOCK CONTAINS      31   RECORDS
                        LABEL RECORD   IS   STANDARD
                        DATA  RECORD   IS   SSIHAREC.
 01  SSIHAREC.
*
     03  SSIHA-REC01              PIC  X(04).
     03  SSIHA-REC02              PIC  X(124).
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 店舗マスタ >>--*
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  COUNTERS.
     03  LINE-CNT       PIC  9(03).
     03  PAGE-CNT       PIC  9(03).
 01  IDX.
     03  IX             PIC  9(01).
 01  ID-PROGRAM.
     03  PG-ID          PIC  X(08)     VALUE  "SSI1510L".
 01  FLGS.
     03  END-FLG        PIC  X(03)     VALUE  SPACE.
     03  SOUSAI-FLG     PIC  9(01)     VALUE  ZERO.
     03  MEISAI-FLG     PIC  9(01)     VALUE  ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SSI-ST             PIC  X(02).
 01  TOK-ST             PIC  X(02).
 01  TEN-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  WK-DATE            PIC  9(06).
 01  FILLER             REDEFINES      WK-DATE.
     03  WK-YY          PIC  9(02).
     03  WK-MM          PIC  9(02).
     03  WK-DD          PIC  9(02).
*金額変換（１０桁）
 01  WK-SIHARAI         PIC  X(09).
 01  WK-SIHARAI-R       REDEFINES      WK-SIHARAI.
     03  HEN-SIHARAI    PIC  9(09).
*金額変換（６桁）
 01  WK-SOUKIN          PIC  X(06).
 01  WK-SOUKIN-R        REDEFINES      WK-SOUKIN.
     03  HEN-SOUKIN     PIC  9(06).
*取引先変換
 01  WK-TORIHIKI        PIC  X(06).
 01  WK-TORIHIKI-R      REDEFINES      WK-TORIHIKI.
     03  HEN-TORIHIKI   PIC  9(06).
*店舗変換
 01  WK-TENPO           PIC  X(05).
 01  WK-TENPO-R         REDEFINES      WK-TENPO.
     03  HEN-TENPO      PIC  9(05).
*
 01  WK-TENCD           PIC  X(05).
*金額変換ワーク（１０桁）
 01  WK-HENKAN.
     03  WK-HENKAN-1    PIC  X(01)    VALUE  SPACE.
     03  WK-HENKAN-2    PIC  X(09)    VALUE  SPACE.
 01  WK-HENKAN-KIN      PIC S9(10)    VALUE  ZERO.
*金額変換ワーク（６桁）
 01  WK-HENSOU.
     03  WK-HENSOU-1    PIC  X(01)    VALUE  SPACE.
     03  WK-HENSOU-2    PIC  X(05)    VALUE  SPACE.
 01  WK-HENSOU-KIN      PIC S9(06)    VALUE  ZERO.
*差引支払額エリア
 01  WK-KEISAN-AREA.
     03  WK-SASIHIKI    PIC S9(10)    VALUE  ZERO.
     03  WK-SIHAKIN     PIC S9(10)    VALUE  ZERO.
     03  WK-SOUSAI      PIC S9(10)    VALUE  ZERO.
*支払合計情報格納
 01  SIHARAI-JYOUHO.
     03  GK-TEN-KEI     PIC S9(10)    VALUE  ZERO.
     03  GK-SOTO-KEI    PIC S9(10)    VALUE  ZERO.
     03  GK-UTI-KEI     PIC S9(10)    VALUE  ZERO.
     03  GK-HIKA-KEI    PIC S9(10)    VALUE  ZERO.
*
 01  HED1-REC.
     03  HED1-F01                 PIC  X(01).
     03  HED1-F02                 PIC  X(02).
     03  HED1-F03                 PIC  X(01).
     03  HED1-F04                 PIC  9(06).
     03  HED1-F05                 PIC  9(06).
     03  HED1-F06                 PIC  X(10).
     03  HED1-F07                 PIC  X(10).
     03  HED1-F08                 PIC  X(10).
     03  HED1-F09                 PIC  X(10).
     03  HED1-F10                 PIC  X(10).
     03  HED1-F11                 PIC  X(10).
     03  HED1-F12                 PIC  X(50).
     03  HED1-F13                 PIC  X(02).
*
 01  HED2-REC.
     03  HED2-F01                 PIC  X(01).
     03  HED2-F02                 PIC  X(02).
     03  HED2-F03                 PIC  X(01).
     03  HED2-F04.
         05  HED2-F041            PIC  X(06).
         05  HED2-F042            PIC  X(06).
         05  HED2-F043            PIC  X(10).
         05  HED2-F044            PIC  X(06).
         05  HED2-F045            PIC  X(10).
         05  HED2-F046            PIC  X(10).
     03  HED2-F05.
         05  HED2-F051            PIC  X(06).
         05  HED2-F052            PIC  X(06).
         05  HED2-F053            PIC  X(10).
         05  HED2-F054            PIC  X(06).
         05  HED2-F055            PIC  X(10).
         05  HED2-F056            PIC  X(10).
     03  HED2-F06                 PIC  X(28).
*
 01  WK-HED2-REC.
     03  WK-HED2-REC              OCCURS   10.
         05  WK-HED2-F01          PIC  9(06).
         05  WK-HED2-F02          PIC  9(06).
         05  WK-HED2-F03          PIC  X(10).
         05  WK-HED2-F04          PIC  9(06).
         05  WK-HED2-F05          PIC  X(10).
         05  WK-HED2-F06          PIC  X(10).
*
 01  MEI1-REC.
     03  MEI1-F01                 PIC  X(01).
     03  MEI1-F02                 PIC  X(02).
     03  MEI1-F03                 PIC  X(01).
     03  MEI1-F04                 OCCURS  2.
         05  MEI1-F041            PIC  X(09).
         05  MEI1-F042            PIC  X(05).
         05  MEI1-F043            PIC  X(04).
         05  MEI1-F044            PIC  X(02).
         05  MEI1-F045            PIC  X(06).
         05  MEI1-F046            PIC  X(06).
         05  MEI1-F047            PIC  X(10).
         05  MEI1-F048            PIC  X(01).
         05  MEI1-F049            PIC  X(01).
         05  MEI1-F040            PIC  X(09).
*##2014/04/28 NAV ST
*****03  MEI2-F05                 PIC  X(18).
     03  MEI2-F05A                PIC  X(02).
     03  MEI2-F05B                PIC  X(02).
     03  MEI2-F05                 PIC  X(14).
*##2014/04/28 NAV ED
*
 01  MEI2-REC.
     03  MEI2-F01                 PIC  X(01).
     03  MEI2-F02                 PIC  X(02).
     03  MEI2-F03                 PIC  X(01).
     03  MEI2-F04                 OCCURS  2.
         05  MEI2-F041            PIC  X(02).
         05  MEI2-F042            PIC  X(06).
         05  MEI2-F043            PIC  X(06).
         05  MEI2-F044            PIC  X(10).
         05  MEI2-F045            PIC  X(25).
     03  MEI2-F05                 PIC  X(26).
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  NEW.
         05  NEW-TEN                        PIC  X(05).
         05  NEW-TOR                        PIC  X(09).
     03  OLD.
         05  OLD-TEN                        PIC  X(05).
         05  OLD-TOR                        PIC  X(09).
*
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HEAD01.
     03  FILLER         PIC  X(40)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE BAIKAKU-15.
         05  FILLER     PIC  N(20)     VALUE
             NC"＜＜　_セキチュー　支払明細書　植物＞＞".
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(15)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"処理日".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD1-011    PIC  99.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD1-012    PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD1-013    PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"頁".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD1-02     PIC  ZZ9.
 01  HEAD02.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"注　釈：".
         05  HD2-01     PIC  X(50).
 01  HEAD03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"締　日：".
         05  HD3-011    PIC  99.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD3-012    PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD3-013    PIC  Z9.
 01  HEAD04.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(06)     VALUE  NC"＜買掛情報＞".
         05  FILLER     PIC  X(40)     VALUE
             ALL "=======================================".
         05  FILLER     PIC  X(40)     VALUE
             ALL "=======================================".
         05  FILLER     PIC  X(42)     VALUE
             ALL "==========================================".
 01  HEAD05.
     03  FILLER         PIC  X(02)     VALUE  SPACE.
     03  FILLER         PIC  X(48)     VALUE
       ALL "================================================".
     03  FILLER         PIC  X(48)     VALUE
       ALL "================================================".
     03  FILLER         PIC  X(40)     VALUE
       ALL "========================================".
 01  HEAD06.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(10)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"前月残高".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"当月仕入高".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"消費税".
         05  FILLER     PIC  X(01)     VALUE  "(".
         05  FILLER     PIC  N(01)     VALUE  NC"外".
         05  FILLER     PIC  X(01)     VALUE  ")".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"消費税".
         05  FILLER     PIC  X(01)     VALUE  "(".
         05  FILLER     PIC  N(01)     VALUE  NC"内".
         05  FILLER     PIC  X(01)     VALUE  ")".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"当月支払高".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"当月残高".
 01  HEAD07.
     03  FILLER         PIC  X(05)     VALUE  SPACE.
     03  HD07-01        PIC  -,---,---,--9.
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  HD07-02        PIC  -,---,---,--9.
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  HD07-03        PIC  -,---,---,--9.
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  HD07-04        PIC  -,---,---,--9.
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  HD07-05        PIC  -,---,---,--9.
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  HD07-06        PIC  -,---,---,--9.
 01  HEAD08.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(08)     VALUE
             NC"＜支払予定情報＞".
         05  FILLER     PIC  X(40)     VALUE
             ALL "========================================".
         05  FILLER     PIC  X(40)     VALUE
             ALL "========================================".
         05  FILLER     PIC  X(38)     VALUE
             ALL "======================================".
 01  HEAD09.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(10)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"支払予定日".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"買掛発生日".
         05  FILLER     PIC  X(06)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"買掛金額".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"相殺発生日".
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"相殺額".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(07)    VALUE  NC"差引支払予定額".
 01  HEAD10.
     03  FILLER         PIC  X(11)     VALUE  SPACE.
     03  HD10-01        PIC  99.
     03  HD10-02        PIC  X(01).
     03  HD10-03        PIC  Z9.
     03  HD10-04        PIC  X(01).
     03  HD10-05        PIC  Z9.
     03  FILLER         PIC  X(04).
     03  HD10-06        PIC  99.
     03  HD10-07        PIC  X(01).
     03  HD10-08        PIC  Z9.
     03  HD10-09        PIC  X(01).
     03  HD10-10        PIC  Z9.
     03  FILLER         PIC  X(02)     VALUE  SPACE.
     03  HD10-11        PIC  -,---,---,--9.
     03  FILLER         PIC  X(04).
     03  HD10-12        PIC  99.
     03  HD10-13        PIC  X(01).
     03  HD10-14        PIC  Z9.
     03  HD10-15        PIC  X(01).
     03  HD10-16        PIC  Z9.
     03  FILLER         PIC  X(02)     VALUE  SPACE.
     03  HD10-17        PIC  -,---,---,--9.
     03  FILLER         PIC  X(05)     VALUE  SPACE.
     03  HD10-18        PIC  -,---,---,--9.
 01  HEAD11.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"伝票区分".
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"店舗".
         05  FILLER     PIC  X(21)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"部門".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"伝票番号".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"検収日".
         05  FILLER     PIC  X(04)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"支払日".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"支払金額".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"税".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"訂".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"元伝票番号".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(02)     VALUE  NC"備考".
*
 01  MEIS01.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  MS01-01    PIC  X(02).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-02    PIC  N(06).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-03    PIC  999.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-04    PIC  N(10).
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS01-05    PIC  999.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS01-06    PIC  999999999.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS01-07    PIC  99.
         05  MS01-08    PIC  X(01).
         05  MS01-09    PIC  Z9.
         05  MS01-10    PIC  X(01).
         05  MS01-11    PIC  Z9.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS01-12    PIC  99.
         05  MS01-13    PIC  X(01).
         05  MS01-14    PIC  Z9.
         05  MS01-15    PIC  X(01).
         05  MS01-16    PIC  Z9.
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  MS01-17    PIC  -,---,---,--9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-18    PIC  N(01).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-19    PIC  N(01).
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-20    PIC  999999999.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  MS01-21    PIC  X(25).
 01  TAIL01.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(71)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"（小計）".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  TL01-01    PIC  -,---,---,--9.
 01  TAIL02.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(67)     VALUE  SPACE.
         05  FILLER     PIC  N(06)     VALUE  NC"（合計・外）".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  TL02-01    PIC  -,---,---,--9.
 01  TAIL03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(67)     VALUE  SPACE.
         05  FILLER     PIC  N(06)     VALUE  NC"（合計・内）".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  TL03-01    PIC  -,---,---,--9.
 01  TAIL04.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(67)     VALUE  SPACE.
         05  FILLER     PIC  N(06)     VALUE  NC"（合計・非）".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  TL04-01    PIC  -,---,---,--9.
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 支払情報データ >>--*
 SHI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SSIHARAD.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### " PG-ID "  SSIHARAD  ERROR " SSI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### " PG-ID "  HTOKMS   ERROR " TOK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### " PG-ID "  HTENMS   ERROR " TEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
*スタートメッセージ
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "***   " PG-ID " START  *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL  END-FLG = "END".
     PERFORM  300-END-RTN.
*
*終了メッサージ出力
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "***   " PG-ID " END    *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
*ファイルのオープン
     OPEN     INPUT     SSIHARAD.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     HTENMS.
     OPEN     OUTPUT    PRTF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         COUNTERS.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
*支払情報データ初期読込み
     PERFORM  900-SSI-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*ファイルのクローズ
     IF       PAGE-CNT  >  ZERO
              PERFORM  900-KEI1-PRT-RTN
              PERFORM  900-KEI2-PRT-RTN
              PERFORM  900-KEI3-PRT-RTN
              PERFORM  900-KEI4-PRT-RTN
     END-IF.
     CLOSE    SSIHARAD.
     CLOSE    HTOKMS.
     CLOSE    HTENMS.
     CLOSE    PRTF.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ヘッダ情報付加                              *
*--------------------------------------------------------------*
 900-HED1-PRT-RTN       SECTION.
*ヘッダ行印字
     PERFORM  211-HEAD-PRINT.
*注釈
     MOVE     HED1-F12       TO   HD2-01.
*締日
     MOVE     HED1-F05(1:2)  TO   HD3-011.
     MOVE     HED1-F05(3:2)  TO   HD3-012.
     MOVE     HED1-F05(5:2)  TO   HD3-013.
*前月残高
     MOVE     SPACE          TO   WK-HENKAN.
     MOVE     HED1-F06       TO   WK-HENKAN.
     PERFORM  900-HENKAN-RTN.
     MOVE     WK-HENKAN-KIN  TO   HD07-01.
*当月仕入
     MOVE     SPACE          TO   WK-HENKAN.
     MOVE     HED1-F07       TO   WK-HENKAN.
     PERFORM  900-HENKAN-RTN.
     MOVE     WK-HENKAN-KIN  TO   HD07-02.
*当月消費税（外）
     MOVE     SPACE          TO   WK-HENKAN.
     MOVE     HED1-F08       TO   WK-HENKAN.
     PERFORM  900-HENKAN-RTN.
     MOVE     WK-HENKAN-KIN  TO   HD07-03.
*当月消費税（内）
     MOVE     SPACE          TO   WK-HENKAN.
     MOVE     HED1-F09       TO   WK-HENKAN.
     PERFORM  900-HENKAN-RTN.
     MOVE     WK-HENKAN-KIN  TO   HD07-04.
*当月支払
     MOVE     SPACE          TO   WK-HENKAN.
     MOVE     HED1-F10       TO   WK-HENKAN.
     PERFORM  900-HENKAN-RTN.
     MOVE     WK-HENKAN-KIN  TO   HD07-05.
*当月残高
     MOVE     SPACE          TO   WK-HENKAN.
     MOVE     HED1-F11       TO   WK-HENKAN.
     PERFORM  900-HENKAN-RTN.
     MOVE     WK-HENKAN-KIN  TO   HD07-06.
*ＨＥＡＤ出力
     WRITE    PRT-REC   FROM      HEAD02    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD03    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD04    AFTER     2.
     WRITE    PRT-REC   FROM      HEAD06    AFTER     2.
     WRITE    PRT-REC   FROM      HEAD07    AFTER     2.
     WRITE    PRT-REC   FROM      HEAD05    AFTER     2.
     WRITE    PRT-REC   FROM      HEAD08    AFTER     2.
     WRITE    PRT-REC   FROM      HEAD09    AFTER     2.
     ADD      14             TO   LINE-CNT.
*
 900-HED1-PRT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3      ヘッダ情報付加                              *
*--------------------------------------------------------------*
 900-HED2-PRT-RTN       SECTION.
*初期化
     MOVE     SPACE          TO   HEAD10.
*支払予定日セット
     IF       HED2-F041  NOT =  SPACE
              MOVE     HED2-F041      TO   WK-SOUKIN
              MOVE    HEN-SOUKIN(1:2) TO   HD10-01
              MOVE    HEN-SOUKIN(3:2) TO   HD10-03
              MOVE    HEN-SOUKIN(5:2) TO   HD10-05
              MOVE    "/"             TO   HD10-02 HD10-04
     END-IF.
*買掛発生日セット
     IF       HED2-F042  NOT =  SPACE
              MOVE     HED2-F042      TO   WK-SOUKIN
              MOVE    HEN-SOUKIN(1:2) TO   HD10-06
              MOVE    HEN-SOUKIN(3:2) TO   HD10-08
              MOVE    HEN-SOUKIN(5:2) TO   HD10-10
              MOVE    "/"             TO   HD10-07 HD10-09
     END-IF.
*買掛金額
     MOVE     SPACE          TO   WK-HENKAN.
     MOVE     HED2-F043      TO   WK-HENKAN.
     PERFORM  900-HENKAN-RTN.
     MOVE     WK-HENKAN-KIN  TO   HD10-11.
*相殺発生日セット
     IF       HED2-F044  NOT =  SPACE
              MOVE     HED2-F044      TO   WK-SOUKIN
              MOVE    HEN-SOUKIN(1:2) TO   HD10-12
              MOVE    HEN-SOUKIN(3:2) TO   HD10-14
              MOVE    HEN-SOUKIN(5:2) TO   HD10-16
              MOVE    "/"             TO   HD10-13 HD10-15
     END-IF.
*相殺金額
     MOVE     SPACE          TO   WK-HENKAN.
     MOVE     HED2-F045      TO   WK-HENKAN.
     PERFORM  900-HENKAN-RTN.
     MOVE     WK-HENKAN-KIN  TO   HD10-17.
*差引支払予定額
     MOVE     SPACE          TO   WK-HENKAN.
     MOVE     HED2-F046      TO   WK-HENKAN.
     PERFORM  900-HENKAN-RTN.
     MOVE     WK-HENKAN-KIN  TO   HD10-18.
*ＨＥＡＤ出力
     WRITE    PRT-REC   FROM      HEAD10    AFTER     1.
     ADD      1              TO   LINE-CNT.
*
     IF       HED2-F055  NOT =  SPACE
*          DISPLAY "###-AAAAA#"
*          初期化
           MOVE     SPACE          TO   HEAD10
*          支払予定日セット
           IF       HED2-F051  NOT =  SPACE
              MOVE     HED2-F051      TO   WK-SOUKIN
              MOVE    HEN-SOUKIN(1:2) TO   HD10-01
              MOVE    HEN-SOUKIN(3:2) TO   HD10-03
              MOVE    HEN-SOUKIN(5:2) TO   HD10-05
              MOVE    "/"             TO   HD10-02 HD10-04
           END-IF
*          買掛発生日セット
           IF       HED2-F052  NOT =  SPACE
              MOVE     HED2-F052      TO   WK-SOUKIN
              MOVE    HEN-SOUKIN(1:2) TO   HD10-06
              MOVE    HEN-SOUKIN(3:2) TO   HD10-08
              MOVE    HEN-SOUKIN(5:2) TO   HD10-10
              MOVE    "/"             TO   HD10-07 HD10-09
           END-IF
*          買掛金額
           MOVE     SPACE          TO   WK-HENKAN
           MOVE     HED2-F053      TO   WK-HENKAN
           PERFORM  900-HENKAN-RTN
           MOVE     WK-HENKAN-KIN  TO   HD10-11
*          相殺発生日セット
           IF       HED2-F054  NOT =  SPACE
              MOVE     HED2-F054      TO   WK-SOUKIN
              MOVE    HEN-SOUKIN(1:2) TO   HD10-12
              MOVE    HEN-SOUKIN(3:2) TO   HD10-14
              MOVE    HEN-SOUKIN(5:2) TO   HD10-16
              MOVE    "/"             TO   HD10-13 HD10-15
           END-IF
*          相殺金額
           MOVE     SPACE          TO   WK-HENKAN
           MOVE     HED2-F055      TO   WK-HENKAN
           PERFORM  900-HENKAN-RTN
           MOVE     WK-HENKAN-KIN  TO   HD10-17
*          差引支払予定額
           MOVE     SPACE          TO   WK-HENKAN
           MOVE     HED2-F056      TO   WK-HENKAN
           PERFORM  900-HENKAN-RTN
           MOVE     WK-HENKAN-KIN  TO   HD10-18
*          ＨＥＡＤ出力
           WRITE    PRT-REC   FROM      HEAD10    AFTER     1
           ADD      1              TO   LINE-CNT
     END-IF.
*
 900-HED2-PRT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     ﾒｲｻｲ ｲﾝｻﾂ（明細行出力）                      *
*--------------------------------------------------------------*
 900-MEISAI1-PRT-RTN    SECTION.
*ラインカウントによる改頁チェック
     IF       LINE-CNT  >  55
              PERFORM   211-HEAD-PRINT
              WRITE     PRT-REC   FROM HEAD05  AFTER  2
              WRITE     PRT-REC   FROM HEAD11  AFTER  1
              WRITE     PRT-REC   FROM HEAD05  AFTER  1
              WRITE     PRT-REC   FROM P-SPACE AFTER  1
     END-IF.
*店舗ＣＤブレイク時、小計出力
     IF       WK-TENCD  NOT =  MEI1-F042(IX)
              PERFORM   900-KEI1-PRT-RTN
              MOVE  MEI1-F042(IX)  TO  WK-TENCD
     END-IF.
*初期化
     MOVE     SPACE              TO   MEIS01.
*伝票区分
     MOVE     MEI1-F044(IX)      TO   MS01-01.
     EVALUATE MEI1-F044(IX)
         WHEN   "51"
         MOVE   NC"定番仕入"     TO   MS01-02
         WHEN   "52"
         MOVE   NC"特売仕入"     TO   MS01-02
         WHEN   "53"
         MOVE   NC"スポット仕入" TO   MS01-02
         WHEN   "54"
         MOVE   NC"客注仕入"     TO   MS01-02
         WHEN   "55"
         MOVE   NC"返　　品"     TO   MS01-02
         WHEN   "56"
         MOVE   NC"値　　引"     TO   MS01-02
         WHEN   "57"
         MOVE   NC"リベート"     TO   MS01-02
         WHEN   "58"
         MOVE   NC"相　　殺"     TO   MS01-02
*## 2016/04/27 NAV ST
         WHEN   "91"
         MOVE   NC"訂正黒　"     TO   MS01-02
         WHEN   "92"
         MOVE   NC"訂正赤　"     TO   MS01-02
*## 2016/04/27 NAV ED
         WHEN   OTHER
         MOVE   NC"　　　　"     TO   MS01-02
     END-EVALUATE.
*店舗コード
     IF  MEI1-F042(IX)  NOT  =    ZERO
              MOVE     MEI1-F042(IX)  TO   WK-TENPO
              MOVE     HEN-TENPO      TO   MS01-03
     END-IF.
*店舗名称取得
     MOVE     321717         TO   TEN-F52.
     MOVE     MEI1-F042(IX)  TO   WK-TENPO.
     MOVE     HEN-TENPO      TO   TEN-F011.
     PERFORM  900-TEN-READ.
     IF       TEN-F03  =  SPACE
              MOVE     ALL NC"＊"     TO   MS01-04
     ELSE
              MOVE     TEN-F03        TO   MS01-04
     END-IF.
*部門
     MOVE     MEI1-F043(IX)  TO   MS01-05.
*伝票_
     MOVE     MEI1-F041(IX)  TO   MS01-06.
*検収日編集
     MOVE     MEI1-F045(IX)  TO   WK-SOUKIN.
     MOVE    HEN-SOUKIN(1:2) TO   MS01-07.
     MOVE     "/"            TO   MS01-08.
     MOVE    HEN-SOUKIN(3:2) TO   MS01-09.
     MOVE     "/"            TO   MS01-10.
     MOVE    HEN-SOUKIN(5:2) TO   MS01-11.
*支払日編集
     MOVE     MEI1-F046(IX)  TO   WK-SOUKIN.
     MOVE    HEN-SOUKIN(1:2) TO   MS01-12.
     MOVE     "/"            TO   MS01-13.
     MOVE    HEN-SOUKIN(3:2) TO   MS01-14.
     MOVE     "/"            TO   MS01-15.
     MOVE    HEN-SOUKIN(5:2) TO   MS01-16.
*支払金額
     MOVE     SPACE          TO   WK-HENKAN.
     MOVE     MEI1-F047(IX)  TO   WK-HENKAN.
     PERFORM  900-HENKAN-RTN.
     MOVE     WK-HENKAN-KIN  TO   MS01-17.
*****ADD      WK-HENKAN-KIN  TO   GK-TEN-KEI.
*## 2016/04/27 NAV ST
*****IF       MEI1-F044(IX)  =  "51" OR "52" OR "53" OR "54"
     IF       MEI1-F044(IX)  =  "51" OR "52" OR "53" OR "54" OR
                                "91"
*## 2016/04/27 NAV ED
              ADD WK-HENKAN-KIN TO GK-TEN-KEI
     ELSE
              COMPUTE MS01-17 = WK-HENKAN-KIN * -1
              COMPUTE WK-HENKAN-KIN = WK-HENKAN-KIN * -1
              ADD WK-HENKAN-KIN TO GK-TEN-KEI
     END-IF.
*税区分
     EVALUATE MEI1-F048(IX)
         WHEN "0"  MOVE NC"外" TO MS01-18
                   ADD  WK-HENKAN-KIN TO GK-SOTO-KEI
         WHEN "1"  MOVE NC"内" TO MS01-18
                   ADD  WK-HENKAN-KIN TO GK-UTI-KEI
         WHEN "2"  MOVE NC"非" TO MS01-18
                   ADD  WK-HENKAN-KIN TO GK-HIKA-KEI
     END-EVALUATE.
*訂正区分
     EVALUATE MEI1-F049(IX)
         WHEN "0"  MOVE NC"　" TO MS01-19
         WHEN "1"  MOVE NC"訂" TO MS01-19
     END-EVALUATE.
*訂正元伝票番号
     MOVE     MEI1-F040(IX)  TO   MS01-20.
*備考
*****MOVE     SPACE          TO   MS01-21.
*##2014/04/28 NAV ST
*****DISPLAY "IX = " IX UPON CONS.
     EVALUATE IX
         WHEN 1
         MOVE "ZEI(%):"      TO   MS01-21(1:7)
         MOVE MEI2-F05A      TO   MS01-21(8:2)
         WHEN 2
         MOVE "ZEI(%):"      TO   MS01-21(1:7)
         MOVE MEI2-F05B      TO   MS01-21(8:2)
         WHEN OTHER
         MOVE SPACE          TO   MS01-21
     END-EVALUATE.
*##2014/04/28 NAV ED
*明細行印字
     WRITE    PRT-REC   FROM  MEIS01   AFTER     1.
     ADD      1              TO   LINE-CNT.
*
 900-MEISAI1-PRN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     ﾒｲｻｲ ｲﾝｻﾂ（明細行出力）                      *
*--------------------------------------------------------------*
 900-MEISAI2-PRT-RTN    SECTION.
*ラインカウントによる改頁チェック
     IF       LINE-CNT  >  55
              PERFORM   211-HEAD-PRINT
              WRITE     PRT-REC   FROM HEAD05  AFTER  2
              WRITE     PRT-REC   FROM HEAD11  AFTER  1
              WRITE     PRT-REC   FROM HEAD05  AFTER  1
              WRITE     PRT-REC   FROM P-SPACE AFTER  1
     END-IF.
*店舗ＣＤブレイク時、小計出力
*****IF       WK-TENCD  NOT =  MEI1-F042(IX)
*****         PERFORM   900-KEI1-PRT-RTN
*****         MOVE  MEI1-F042(IX)  TO  WK-TENCD
*****END-IF.
*初期化
     MOVE     SPACE          TO   MEIS01.
*伝票区分
     MOVE     "58"           TO   MS01-01.
     MOVE     NC"相　　殺"   TO   MS01-02.
*伝票_
     MOVE     MEI2-F041(IX)  TO   MS01-06.
*検収日編集
     MOVE     MEI2-F042(IX)  TO   WK-SOUKIN.
     MOVE    HEN-SOUKIN(1:2) TO   MS01-07.
     MOVE     "/"            TO   MS01-08.
     MOVE    HEN-SOUKIN(3:2) TO   MS01-09.
     MOVE     "/"            TO   MS01-10.
     MOVE    HEN-SOUKIN(5:2) TO   MS01-11.
*支払日編集
     MOVE     MEI2-F043(IX)  TO   WK-SOUKIN.
     MOVE    HEN-SOUKIN(1:2) TO   MS01-12.
     MOVE     "/"            TO   MS01-13.
     MOVE    HEN-SOUKIN(3:2) TO   MS01-14.
     MOVE     "/"            TO   MS01-15.
     MOVE    HEN-SOUKIN(5:2) TO   MS01-16.
*支払金額
     MOVE     SPACE          TO   WK-HENKAN.
     MOVE     MEI2-F044(IX)  TO   WK-HENKAN.
     PERFORM  900-HENKAN-RTN.
     COMPUTE  WK-HENKAN-KIN = WK-HENKAN-KIN * -1.
     MOVE     WK-HENKAN-KIN  TO   MS01-17.
     ADD      WK-HENKAN-KIN  TO   GK-TEN-KEI.
     ADD      WK-HENKAN-KIN  TO   GK-SOTO-KEI.
*備考
     MOVE     MEI2-F045(IX)  TO   MS01-21.
*明細行印字
     WRITE    PRT-REC   FROM  MEIS01   AFTER     1.
     ADD      1              TO   LINE-CNT.
*
 900-MEISAI2-PRN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    小計印字　　　　　　　　　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 900-KEI1-PRT-RTN       SECTION.
*
     MOVE     GK-TEN-KEI     TO   TL01-01.
     WRITE    PRT-REC        FROM TAIL01    AFTER     1.
     ADD      1              TO   LINE-CNT.
     MOVE     ZERO           TO   GK-TEN-KEI.
*
 900-KEI1-PRT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    合計・外税合計印字　　　　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 900-KEI2-PRT-RTN       SECTION.
*
     MOVE     GK-SOTO-KEI    TO   TL02-01.
     WRITE    PRT-REC        FROM TAIL02    AFTER     1.
     ADD      1              TO   LINE-CNT.
*
 900-KEI2-PRT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    合計・内税合計印字　　　　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 900-KEI3-PRT-RTN       SECTION.
*
     MOVE     GK-UTI-KEI     TO   TL03-01.
     WRITE    PRT-REC        FROM TAIL03    AFTER     1.
     ADD      1              TO   LINE-CNT.
*
 900-KEI3-PRT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    合計・非課税合計印字　　　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 900-KEI4-PRT-RTN       SECTION.
*
     MOVE     GK-HIKA-KEI    TO   TL04-01.
     WRITE    PRT-REC        FROM TAIL04    AFTER     1.
     ADD      1              TO   LINE-CNT.
*
 900-KEI4-PRT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       ﾀｲﾄﾙ ﾌﾟﾘﾝﾄ ｼｮﾘ（ＨＥＡＤプリント）          *
*--------------------------------------------------------------*
 211-HEAD-PRINT         SECTION.
*改頁制御
     IF       PAGE-CNT  >    1
              WRITE     PRT-REC   FROM P-SPACE   AFTER  PAGE
              MOVE      ZERO      TO   LINE-CNT
     END-IF.
*ページカウンター
     ADD      1              TO   PAGE-CNT.
*システム日付
     MOVE     SYS-YY         TO   HD1-011.
     MOVE     SYS-MM         TO   HD1-012.
     MOVE     SYS-DD         TO   HD1-013.
*頁
     MOVE     PAGE-CNT       TO   HD1-02.
*ＨＥＡＤ出力
     WRITE    PRT-REC   FROM      HEAD01    AFTER     2.
     MOVE     2              TO   LINE-CNT.
*
 211-HEAD-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
 900-TOK-READ           SECTION.
     READ     HTOKMS    INVALID
              MOVE      SPACE          TO   TOK-F03
     END-READ.
 900-TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    店舗マスタ　　READ                           *
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
     READ     HTENMS    INVALID
              MOVE      SPACE          TO   TEN-F03
     END-READ.
 900-TEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    支払情報データ　　 READ                      *
*--------------------------------------------------------------*
 900-SSI-READ           SECTION.
     READ     SSIHARAD  AT   END
              MOVE      "END"        TO     END-FLG
              GO   TO   900-SSI-READ-EXIT
     END-READ.
*
     IF   SSIHA-REC01  =  "H21K"
          PERFORM 900-HED1-RTN
          PERFORM 900-HED1-PRT-RTN
          GO                           TO   900-SSI-READ
     END-IF.
*
     IF   SSIHA-REC01  =  "H21Y"
          PERFORM 900-HED2-RTN
          PERFORM 900-HED2-PRT-RTN
          MOVE    1                    TO   SOUSAI-FLG
          GO                           TO   900-SSI-READ
     ELSE
          IF      SOUSAI-FLG  =  1
                  WRITE     PRT-REC   FROM HEAD05  AFTER  2
                  WRITE     PRT-REC   FROM P-SPACE AFTER  PAGE
                  PERFORM   211-HEAD-PRINT
                  WRITE     PRT-REC   FROM HEAD05  AFTER  2
                  WRITE     PRT-REC   FROM HEAD11  AFTER  1
                  WRITE     PRT-REC   FROM HEAD05  AFTER  1
                  WRITE     PRT-REC   FROM P-SPACE AFTER  1
                  MOVE      5         TO   LINE-CNT
                  MOVE      ZERO      TO   SOUSAI-FLG
                  MOVE      SSIHAREC  TO   MEI1-REC
                  MOVE      MEI1-F042(IX)  TO  WK-TENCD
          END-IF
     END-IF.
*
     IF   SSIHA-REC01  =  "B21M"
          PERFORM 900-MEI1-RTN
          MOVE    1        TO      MEISAI-FLG
     END-IF.
*
     IF   SSIHA-REC01  =  "B21S"
          IF   MEISAI-FLG  =  1
               PERFORM 900-KEI1-PRT-RTN
               MOVE    ZERO     TO      MEISAI-FLG
          END-IF
          PERFORM 900-MEI2-RTN
     END-IF.
*
 900-SSI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    金額編集処理                                 *
*--------------------------------------------------------------*
 900-HENKAN-RTN         SECTION.
*
     IF       WK-HENKAN-1    =    "-"
              MOVE WK-HENKAN-2 TO WK-SIHARAI
              COMPUTE WK-HENKAN-KIN = HEN-SIHARAI * (-1)
     ELSE
              MOVE WK-HENKAN-2 TO WK-SIHARAI
              MOVE HEN-SIHARAI TO WK-HENKAN-KIN
     END-IF.
*
 900-HENKAN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    金額編集処理                                 *
*--------------------------------------------------------------*
 900-HENSOU-RTN         SECTION.
*
     IF       WK-HENSOU-1    =    "-"
              MOVE WK-HENSOU-2 TO WK-SOUKIN
              COMPUTE WK-HENSOU-KIN = HEN-SOUKIN * (-1)
     ELSE
              MOVE WK-HENSOU-2 TO WK-SOUKIN
              MOVE HEN-SOUKIN  TO WK-HENSOU-KIN
     END-IF.
*
 900-HENSOU-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    買掛金残高データ                             *
*--------------------------------------------------------------*
 900-HED1-RTN           SECTION.
*ワーク初期化
     MOVE     SPACE          TO   HED1-REC.
     INITIALIZE                   HED1-REC.
*レコード転送
     MOVE     SSIHAREC       TO   HED1-REC.
*
 900-HED1-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    支払予定データ                               *
*--------------------------------------------------------------*
 900-HED2-RTN           SECTION.
*ワーク初期化
     MOVE     SPACE          TO   HED2-REC.
     INITIALIZE                   HED2-REC.
*レコード転送
     MOVE     SSIHAREC       TO   HED2-REC.
*
 900-HED2-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    買掛金残高データ                             *
*--------------------------------------------------------------*
 900-MEI1-RTN           SECTION.
*ワーク初期化
     MOVE     SPACE          TO   MEI1-REC.
     INITIALIZE                   MEI1-REC.
*レコード転送
     MOVE     SSIHAREC       TO   MEI1-REC.
*レコード分割
     PERFORM  VARYING  IX  FROM  1  BY  1  UNTIL  IX  >  2
              IF    MEI1-F041(IX)  NOT =  SPACE
                    PERFORM 900-MEISAI1-PRT-RTN
              END-IF
     END-PERFORM.
*
 900-MEI1-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    買掛金残高データ                             *
*--------------------------------------------------------------*
 900-MEI2-RTN           SECTION.
*ワーク初期化
     MOVE     SPACE          TO   MEI2-REC.
     INITIALIZE                   MEI2-REC.
*レコード転送
     MOVE     SSIHAREC       TO   MEI2-REC.
*レコード分割
     PERFORM  VARYING  IX  FROM  1  BY  1  UNTIL  IX  >  2
              IF    MEI2-F041(IX)  NOT =  SPACE
                    PERFORM 900-MEISAI2-PRT-RTN
              END-IF
     END-PERFORM.
*
 900-MEI2-RTN-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

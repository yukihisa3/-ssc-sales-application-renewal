****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　セキチュー支払照合　　　　　　　　*
*    モジュール名　　　　：　支払明細表作成　　　　　　　　　　*
*    作成日／更新日　　　：　95/11/15                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　支払情報ファイルより支払明細表を　*
*                        ：　印刷する。　　　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSI0110L.
 AUTHOR.                T.T.
 DATE-WRITTEN.          96/04/05.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
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
 FD  SSIHARAD           LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS  4   RECORDS.
     COPY     SITGKFD   OF        XFDLIB
              JOINING   SSI       PREFIX.
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
     03  I              PIC  9(03).
 01  ID-PROGRAM.
     03  PG-ID          PIC  X(08)     VALUE  "SSI0110L".
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
 01  WK-SOUKIN          PIC  X(05).
 01  WK-SOUKIN-R        REDEFINES      WK-SOUKIN.
     03  HEN-SOUKIN     PIC  9(05).
*取引先変換
 01  WK-TORIHIKI        PIC  X(06).
 01  WK-TORIHIKI-R      REDEFINES      WK-TORIHIKI.
     03  HEN-TORIHIKI   PIC  9(06).
*店舗変換
 01  WK-TENPO           PIC  X(05).
 01  WK-TENPO-R         REDEFINES      WK-TENPO.
     03  HEN-TENPO      PIC  9(05).
*
 01  GK-SIHARAI         PIC S9(12)V99.
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
*支払情報格納
 01  SIHARAI-JYOUHO.
     03  SIH-SIMEBI     PIC  9(06)    VALUE  ZERO.
     03  SIH-ZENZAN     PIC  X(10)    VALUE  SPACE.
     03  SIH-TOUSII     PIC  X(10)    VALUE  SPACE.
     03  SIH-TOUSYO     PIC  X(10)    VALUE  SPACE.
     03  SIH-TOUSIH     PIC  X(10)    VALUE  SPACE.
     03  SIH-TOUZAN     PIC  X(10)    VALUE  SPACE.
*過去支払情報格納
 01  SIHARAI-OLD.
     03  OLD-1.
         05  YOTEI-1    PIC  9(06)    VALUE  ZERO.
         05  SIHARAI-1  PIC  X(10)    VALUE  SPACE.
         05  SOUSAI-1   PIC  X(10)    VALUE  SPACE.
     03  OLD-2.
         05  YOTEI-2    PIC  9(06)    VALUE  ZERO.
         05  SIHARAI-2  PIC  X(10)    VALUE  SPACE.
         05  SOUSAI-2   PIC  X(10)    VALUE  SPACE.
     03  OLD-3.
         05  YOTEI-3    PIC  9(06)    VALUE  ZERO.
         05  SIHARAI-3  PIC  X(10)    VALUE  SPACE.
         05  SOUSAI-3   PIC  X(10)    VALUE  SPACE.
*
*----<< ｺﾞｳｹｲ ﾜｰｸ >>--*
 01  GOKEI-AREA.
     03  KEI-TBL        OCCURS    3.
         05  KEI-KIN    PIC  S9(12)V99   PACKED-DECIMAL.
*
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
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"取引先".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD-03      PIC  9(08).
         05  FILLER     PIC  X(08)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE BAIKAKU-15.
         05  FILLER     PIC  N(09)     VALUE
                        NC"【　支払明細書　】".
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"処理日".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD-011     PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD-012     PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  "/".
         05  HD-013     PIC  Z9.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"頁".
         05  FILLER     PIC  X(01)     VALUE  ":".
         05  HD-02      PIC  ZZ9.
 01  HEAD02.
     03  FILLER         CHARACTER TYPE PITCH-15.
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  HD-04      PIC  N(10).
 01  HEAD03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(55)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"締切日：".
         05  HD-061     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  HD-062     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
         05  HD-063     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
*********05  FILLER     PIC  N(02)     VALUE  NC"現手".
 01  HEAD04.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"項目：".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  HD-05      PIC  X(20).
         05  FILLER     PIC  X(27)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"支払日：".
         05  HD-071     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  HD-072     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
         05  HD-073     PIC  Z9.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
*********05  HD-221     PIC  X(01).
*********05  FILLER     PIC  X(01)     VALUE  SPACE.
*********05  HD-222     PIC  X(01).
 01  HEAD040.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  N(20)     VALUE
             NC"＜前残／当月情報＞___________".
         05  FILLER     PIC  N(20)     VALUE
             NC"____________________".
 01  HEAD041.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(19)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"前月残".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"当月仕入".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(05)     VALUE  NC"当月消費税".
         05  FILLER     PIC  X(05)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"当月支払".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"当月残".
 01  HEAD042.
     03  FILLER         PIC  X(12)     VALUE  SPACE.
     03  HD-042-1       PIC  ----,---,--9.
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  HD-042-2       PIC  ----,---,--9.
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  HD-042-3       PIC  ----,---,--9.
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  HD-042-4       PIC  ----,---,--9.
     03  FILLER         PIC  X(01)     VALUE  SPACE.
     03  HD-042-5       PIC  ----,---,--9.
 01  HEAD043.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  N(20)     VALUE
             NC"____________________".
         05  FILLER     PIC  N(20)     VALUE
             NC"____________________".
 01  HEAD05.
     03  FILLER         CHARACTER TYPE MODE-2.
*********05  FILLER     PIC  X(00)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"伝票区分".
         05  FILLER     PIC  X(03)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"店　舗".
         05  FILLER     PIC  X(16)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"年".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"月".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"日".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"伝票_".
         05  FILLER     PIC  X(08)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"支払金額".
         05  FILLER     PIC  X(02)     VALUE  SPACE.
         05  FILLER     PIC  N(03)     VALUE  NC"備　考".
*
 01  MEIS01.
     03  FILLER         CHARACTER TYPE PITCH-15.
******** 05  FILLER     PIC  X(00).
         05  ME-075     PIC  N(06).
         05  FILLER     PIC  X(02).
         05  ME-08      PIC  9(05).
         05  FILLER     PIC  X(01).
         05  ME-09      PIC  N(10).
         05  FILLER     PIC  X(01).
         05  ME-101     PIC  ZZ.
         05  ME-101P    PIC  X(01).
         05  ME-102     PIC  ZZ.
         05  ME-102P    PIC  X(01).
         05  ME-103     PIC  ZZ.
         05  FILLER     PIC  X(01).
         05  ME-11      PIC  X(09).
         05  FILLER     PIC  X(01).
         05  ME-12      PIC  ----,---,--9.
         05  FILLER     PIC  X(02).
         05  ME-13      PIC  N(10).
 01  MEIS02.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(25)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"＊＊　店".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"合".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"計　＊＊".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  ME-14      PIC  ----,---,--9.
 01  MEIS03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(25)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"＊＊　項".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(01)     VALUE  NC"目".
         05  FILLER     PIC  X(01)     VALUE  SPACE.
         05  FILLER     PIC  N(04)     VALUE  NC"計　＊＊".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  ME-15      PIC  ----,---,--9.
 01  MEIS04.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(25)     VALUE  SPACE.
         05  FILLER     PIC  N(10)
                        VALUE  NC"＊＊　ページ計　＊＊".
         05  FILLER     PIC  X(07)     VALUE  SPACE.
         05  ME-16      PIC  ----,---,--9.
*
 01  TAIL00.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  N(20)     VALUE
             NC"＜支払情報＞______________".
         05  FILLER     PIC  N(20)     VALUE
             NC"____________________".
 01  TAIL01.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(02)  VALUE  SPACE.
         05  FILLER     PIC  N(06)  VALUE  NC"支払予定日".
         05  FILLER     PIC  X(10)  VALUE  SPACE.
         05  FILLER     PIC  N(03)  VALUE  NC"支払額".
         05  FILLER     PIC  X(09)  VALUE  SPACE.
         05  FILLER     PIC  N(03)  VALUE  NC"相殺額".
         05  FILLER     PIC  X(05)  VALUE  SPACE.
         05  FILLER     PIC  N(05)  VALUE  NC"差引支払額".
 01  TAIL02.
         05  FILLER     PIC  X(03).
         05  TA-02-1    PIC  Z9.
         05  FILLER     PIC  X(01)  VALUE  "/".
         05  TA-02-2    PIC  Z9.
         05  FILLER     PIC  X(01)  VALUE  "/".
         05  TA-02-3    PIC  Z9.
         05  FILLER     PIC  X(06)  VALUE  SPACE.
         05  TA-02-4    PIC  ----,---,--9.
         05  FILLER     PIC  X(03)  VALUE  SPACE.
         05  TA-02-5    PIC  ----,---,--9.
         05  FILLER     PIC  X(03)  VALUE  SPACE.
         05  TA-02-6    PIC  ----,---,--9.
 01  TAIL03.
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  N(20)     VALUE
             NC"____________________".
         05  FILLER     PIC  N(20)     VALUE
             NC"____________________".
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
     PERFORM  200-MAIN-RTN   UNTIL     OLD  =    HIGH-VALUE.
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
     MOVE     99             TO   LINE-CNT.
     MOVE     LOW-VALUE      TO   BREAK-KEY.
     INITIALIZE                   GOKEI-AREA.
*支払情報データ初期読込み
     PERFORM  900-SSI-READ.
*支払前回情報退避
     IF       NEW  NOT =  HIGH-VALUE
              MOVE  SSI-F03    TO   SIH-SIMEBI
              MOVE  SSI-F04    TO   SIH-ZENZAN
              MOVE  SSI-F05    TO   SIH-TOUSII
              MOVE  SSI-F06    TO   SIH-TOUSYO
              MOVE  SSI-F07    TO   SIH-TOUSIH
              MOVE  SSI-F08    TO   SIH-TOUZAN
              MOVE  SSI-F09    TO   YOTEI-1
              MOVE  SSI-F10    TO   SIHARAI-1
              MOVE  SSI-F11    TO   SOUSAI-1
              MOVE  SSI-F12    TO   YOTEI-2
              MOVE  SSI-F13    TO   SIHARAI-2
              MOVE  SSI-F14    TO   SOUSAI-2
              MOVE  SSI-F15    TO   YOTEI-3
              MOVE  SSI-F16    TO   SIHARAI-3
              MOVE  SSI-F17    TO   SOUSAI-3
              PERFORM               110-HEAD-HENSYU
     END-IF.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*店舗合計
     IF       OLD       NOT  =    LOW-VALUE
     AND      NEW       NOT  =    OLD
              PERFORM   220-MEIS02-PRINT
     END-IF.
*項目合計
*****IF       OLD       NOT  =    LOW-VALUE
*****AND      NEW-01    NOT  =    OLD-01
*****         PERFORM   230-MEIS03-PRINT
*****END-IF.
*明細　　　　　　　　キーが最大値の時，ページ計出力後終了
     IF       NEW       NOT  =    HIGH-VALUE
              PERFORM   210-MEIS01-PRINT
     ELSE
              IF       PAGE-CNT  >    0
                       PERFORM   240-MEIS04-PRINT
              END-IF
     END-IF.
*キーの入れ替え
     MOVE     NEW            TO   OLD.
*各合計値セット
     IF       NEW  NOT  =    HIGH-VALUE
              PERFORM   260-SYUKEI
                        VARYING   I    FROM 1    BY   1
                        UNTIL     I    >    3
              PERFORM   900-SSI-READ
     END-IF.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*ファイルのクローズ
     PERFORM  900-TAIL-RTN.
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
 110-HEAD-HENSYU        SECTION.
*前月残高
     MOVE     SPACE          TO   WK-HENKAN.
     MOVE     SIH-ZENZAN     TO   WK-HENKAN.
     PERFORM  900-HENKAN-RTN.
     MOVE     WK-HENKAN-KIN  TO   HD-042-1.
*当月仕入
     MOVE     SPACE          TO   WK-HENKAN.
     MOVE     SIH-TOUSII     TO   WK-HENKAN.
     PERFORM  900-HENKAN-RTN.
     MOVE     WK-HENKAN-KIN  TO   HD-042-2.
*当月消費税
     MOVE     SPACE          TO   WK-HENKAN.
     MOVE     SIH-TOUSYO     TO   WK-HENKAN.
     PERFORM  900-HENKAN-RTN.
     MOVE     WK-HENKAN-KIN  TO   HD-042-3.
*当月支払
     MOVE     SPACE          TO   WK-HENKAN.
     MOVE     SIH-TOUSIH     TO   WK-HENKAN.
     PERFORM  900-HENKAN-RTN.
     MOVE     WK-HENKAN-KIN  TO   HD-042-4.
*当月残高
     MOVE     SPACE          TO   WK-HENKAN.
     MOVE     SIH-TOUZAN     TO   WK-HENKAN.
     PERFORM  900-HENKAN-RTN.
     MOVE     WK-HENKAN-KIN  TO   HD-042-5.
*
 110-HEAD-HENSYU-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     ﾒｲｻｲ ｲﾝｻﾂ（明細行出力）                      *
*--------------------------------------------------------------*
 210-MEIS01-PRINT       SECTION.
     IF       LINE-CNT  >    53
              PERFORM   211-HEAD-PRINT
     END-IF.
*
     MOVE     SPACE              TO   MEIS01.
*伝票区分
     EVALUATE SSI-F24
         WHEN   "51"
         MOVE   NC"定番仕入"     TO   ME-075
         WHEN   "52"
         MOVE   NC"特販仕入"     TO   ME-075
         WHEN   "53"
         MOVE   NC"スポット仕入" TO   ME-075
         WHEN   "54"
         MOVE   NC"客注仕入"     TO   ME-075
         WHEN   "55"
         MOVE   NC"返　　品"     TO   ME-075
         WHEN   "56"
         MOVE   NC"値　　引"     TO   ME-075
         WHEN   "57"
         MOVE   NC"リベート"     TO   ME-075
         WHEN   "58"
         MOVE   NC"相　　殺"     TO   ME-075
         WHEN   OTHER
         MOVE   NC"　　　　"     TO   ME-075
     END-EVALUATE.
*店舗コード
     IF       SSI-F22   NOT  =    ZERO
              MOVE     SSI-F22        TO   WK-TENPO
              MOVE     HEN-TENPO      TO   ME-08
     END-IF.
*店舗名称取得
     MOVE     SSI-F25        TO   WK-TORIHIKI.
     MOVE     HEN-TORIHIKI   TO   TEN-F52.
     MOVE     SSI-F22        TO   WK-TENPO.
     MOVE     HEN-TENPO      TO   TEN-F011.
     PERFORM  900-TEN-READ.
     IF       TEN-F03  =  SPACE
              MOVE     ALL NC"＊"     TO   ME-09
     ELSE
              MOVE     TEN-F03        TO   ME-09
     END-IF.
*検収日編集
     MOVE     SSI-F26        TO   WK-DATE.
     MOVE     WK-YY          TO   ME-101.
     MOVE     "."            TO   ME-101P.
     MOVE     WK-MM          TO   ME-102.
     MOVE     "."            TO   ME-102P.
     MOVE     WK-DD          TO   ME-103.
*伝票_
     MOVE     SSI-F21        TO   ME-11.
*支払金額
     MOVE     SPACE          TO   WK-HENKAN.
     MOVE     SSI-F28        TO   WK-HENKAN.
     PERFORM  900-HENKAN-RTN.
     MOVE     WK-HENKAN-KIN  TO   ME-12.
*相殺理由
     EVALUATE SSI-F31
         WHEN "01"
         MOVE NC"基本料金　　　　　　" TO ME-13
         WHEN "02"
         MOVE NC"データ処理料金　　　" TO ME-13
         WHEN "03"
         MOVE NC"倉庫使用料金　　　　" TO ME-13
         WHEN "04"
         MOVE NC"その他　　　　　　　" TO ME-13
         WHEN "11"
         MOVE NC"相殺消費税額　　　　" TO ME-13
         WHEN "91"
         MOVE NC"振込手数料　　　　　" TO ME-13
     END-EVALUATE.
*改行制御
     IF       NEW       NOT  =    OLD
     AND      LINE-CNT  NOT  =    10
              WRITE     PRT-REC   FROM MEIS01    AFTER     2
              ADD       2         TO   LINE-CNT
     ELSE
              WRITE     PRT-REC   FROM MEIS01    AFTER     1
              ADD       1         TO   LINE-CNT
     END-IF.
*
 210-MEIS01-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ﾒｲｻｲ ｲﾝｻﾂ（店舗計）                          *
*--------------------------------------------------------------*
 220-MEIS02-PRINT       SECTION.
     IF       LINE-CNT  >    53
              PERFORM   211-HEAD-PRINT
     END-IF.
*
     MOVE     KEI-KIN (01)   TO   ME-14.
     WRITE    PRT-REC        FROM MEIS02    AFTER     2.
     ADD      2              TO   LINE-CNT.
     INITIALIZE              KEI-TBL (01).
 220-MEIS02-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  5     ﾒｲｻｲ ｲﾝｻﾂ（項目計）                          *
*--------------------------------------------------------------*
 230-MEIS03-PRINT       SECTION.
     IF       LINE-CNT  >    53
              PERFORM   211-HEAD-PRINT
     END-IF.
*
     MOVE     KEI-KIN (02)   TO   ME-15.
     WRITE    PRT-REC        FROM MEIS03    AFTER     2.
     ADD      2              TO   LINE-CNT.
     INITIALIZE              KEI-TBL (02).
 230-MEIS03-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  6     ﾒｲｻｲ ｲﾝｻﾂ（ページ計）                        *
*--------------------------------------------------------------*
 240-MEIS04-PRINT       SECTION.
     MOVE     KEI-KIN (03)   TO   ME-16.
     WRITE    PRT-REC        FROM MEIS04    AFTER     2.
     ADD      2              TO   LINE-CNT.
     INITIALIZE              KEI-TBL (03).
 240-MEIS04-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4     ｼｭｳｹｲ（店舗計，項目計，ページ計集計）        *
*--------------------------------------------------------------*
 260-SYUKEI             SECTION.
*支払金額集計
*支払金額
     MOVE     SPACE          TO    WK-HENKAN.
     MOVE     SSI-F28        TO    WK-HENKAN.
     PERFORM  900-HENKAN-RTN.
     MOVE     WK-HENKAN-KIN  TO    GK-SIHARAI.
     ADD      GK-SIHARAI     TO    KEI-KIN (I).
*
 260-SYUKEI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       ﾀｲﾄﾙ ﾌﾟﾘﾝﾄ ｼｮﾘ（ＨＥＡＤプリント）          *
*--------------------------------------------------------------*
 211-HEAD-PRINT         SECTION.
*ページカウンターがゼロ以外の時，ページ計出力
     IF       OLD       NOT  =    LOW-VALUE
              PERFORM   240-MEIS04-PRINT
     END-IF.
*改頁制御
     IF       PAGE-CNT  >    0
              PERFORM   900-TAIL-RTN
              WRITE     PRT-REC   FROM P-SPACE   AFTER  PAGE
              MOVE      ZERO      TO   LINE-CNT
     END-IF.
*ページカウンター
     ADD      1              TO   PAGE-CNT.
*システム日付
     MOVE     SYS-YY         TO   HD-011.
     MOVE     SYS-MM         TO   HD-012.
     MOVE     SYS-DD         TO   HD-013.
*頁
     MOVE     PAGE-CNT       TO   HD-02.
*取引先コード
     MOVE     SSI-F25        TO   HD-03.
*取引先名取得
     MOVE     SSI-F25        TO   WK-TORIHIKI.
     MOVE     HEN-TORIHIKI   TO   TOK-F01.
     PERFORM  900-TOK-READ.
     MOVE     TOK-F03        TO   HD-04.
*項目名
     MOVE     "ｼｮｳﾋﾝﾀﾞｲｷﾝ"   TO   HD-05.
*締切日
     MOVE     SSI-F03(1:2)   TO   HD-061.
     MOVE     SSI-F03(3:2)   TO   HD-062.
     MOVE     SSI-F03(5:2)   TO   HD-063.
*支払日
     MOVE     SSI-F27(1:2)   TO   HD-071.
     MOVE     SSI-F27(3:2)   TO   HD-072.
     MOVE     SSI-F27(5:2)   TO   HD-073.
*ＨＥＡＤ出力
     WRITE    PRT-REC   FROM      HEAD01    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD02    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD03    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD04    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD040   AFTER     2.
     WRITE    PRT-REC   FROM      HEAD041   AFTER     1.
     WRITE    PRT-REC   FROM      HEAD042   AFTER     1.
     WRITE    PRT-REC   FROM      HEAD043   AFTER     1.
     WRITE    PRT-REC   FROM      HEAD05    AFTER     2.
     WRITE    PRT-REC   FROM      P-SPACE   AFTER     1.
     MOVE     12             TO   LINE-CNT.
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
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   900-SSI-READ-EXIT
     END-READ.
*
     MOVE     SSI-F22        TO   NEW-TEN.
     MOVE     SSI-F25        TO   NEW-TOR.
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
*    LEVEL ALL    テイル印字処理                               *
*--------------------------------------------------------------*
 900-TAIL-RTN           SECTION.
*テイル１行目印字
     WRITE    PRT-REC   FROM      TAIL00    AFTER     2.
     WRITE    PRT-REC   FROM      TAIL01    AFTER     1.
*支払予定日１
     MOVE     YOTEI-1(1:2)   TO   TA-02-1.
     MOVE     YOTEI-1(3:2)   TO   TA-02-2.
     MOVE     YOTEI-1(5:2)   TO   TA-02-3.
*支払額１
     MOVE     SPACE          TO   WK-HENKAN.
     MOVE     SIHARAI-1      TO   WK-HENKAN.
     PERFORM  900-HENKAN-RTN.
     MOVE     WK-HENKAN-KIN  TO   WK-SIHAKIN.
     MOVE     WK-HENKAN-KIN  TO   TA-02-4.
*相殺額１
     MOVE     SPACE          TO   WK-HENSOU.
     MOVE     SOUSAI-1       TO   WK-HENSOU.
*****DISPLAY "SOUSAI-1 = " SOUSAI-1 UPON CONS.
*****DISPLAY "WK-HENSOU-1 = " WK-HENSOU-1 UPON CONS.
*****DISPLAY "WK-HENSOU-2 = " WK-HENSOU-2 UPON CONS.
     PERFORM  900-HENSOU-RTN.
*****DISPLAY "WK-HENSOU-KIN = " WK-HENSOU-KIN UPON CONS.
     MOVE     WK-HENSOU-KIN  TO   WK-SOUSAI.
     MOVE     WK-HENSOU-KIN  TO   TA-02-5.
*差引支払額１
     COMPUTE  WK-SASIHIKI    =    WK-SIHAKIN  -  WK-SOUSAI.
     MOVE     WK-SASIHIKI    TO   TA-02-6.
*１行目印字
     WRITE    PRT-REC   FROM      TAIL02    AFTER     1.
*支払予定日２
     MOVE     YOTEI-2(1:2)   TO   TA-02-1.
     MOVE     YOTEI-2(3:2)   TO   TA-02-2.
     MOVE     YOTEI-2(5:2)   TO   TA-02-3.
*支払額２
     MOVE     SPACE          TO   WK-HENKAN.
     MOVE     SIHARAI-2      TO   WK-HENKAN.
     PERFORM  900-HENKAN-RTN.
     MOVE     WK-HENKAN-KIN  TO   WK-SIHAKIN.
     MOVE     WK-HENKAN-KIN  TO   TA-02-4.
*相殺額２
     MOVE     SPACE          TO   WK-HENSOU.
     MOVE     SOUSAI-2       TO   WK-HENSOU.
     PERFORM  900-HENSOU-RTN.
     MOVE     WK-HENSOU-KIN  TO   WK-SOUSAI.
     MOVE     WK-HENSOU-KIN  TO   TA-02-5.
*差引支払額２
     COMPUTE  WK-SASIHIKI    =    WK-SIHAKIN - WK-SOUSAI.
     MOVE     WK-SASIHIKI    TO   TA-02-6.
*２行目印字
     WRITE    PRT-REC   FROM      TAIL02    AFTER     1.
*支払予定日３
     MOVE     YOTEI-3(1:2)   TO   TA-02-1.
     MOVE     YOTEI-3(3:2)   TO   TA-02-2.
     MOVE     YOTEI-3(5:2)   TO   TA-02-3.
*支払額３
     MOVE     SPACE          TO   WK-HENKAN.
     MOVE     SIHARAI-3      TO   WK-HENKAN.
     PERFORM  900-HENKAN-RTN.
     MOVE     WK-HENKAN-KIN  TO   WK-SIHAKIN.
     MOVE     WK-HENKAN-KIN  TO   TA-02-4.
*相殺額３
     MOVE     SPACE          TO   WK-HENSOU.
     MOVE     SOUSAI-3       TO   WK-HENSOU.
     PERFORM  900-HENSOU-RTN.
     MOVE     WK-HENSOU-KIN  TO   WK-SOUSAI.
     MOVE     WK-HENSOU-KIN  TO   TA-02-5.
*差引支払額３
     COMPUTE  WK-SASIHIKI    =    WK-SIHAKIN - WK-SOUSAI.
     MOVE     WK-SASIHIKI    TO   TA-02-6.
*３行目印字
     WRITE    PRT-REC   FROM      TAIL02    AFTER     1.
     WRITE    PRT-REC   FROM      TAIL03    AFTER     1.
*
 900-TAIL-RTN-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

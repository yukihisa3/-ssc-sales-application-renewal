****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷検品　　　　　　　            *
*    モジュール名　　　　：　出荷確定取込リスト発行　　　　　　*
*    処理概要　　　　　　：　出荷確定累積ファイルより、　　　　*
*                            条件に合致するリストを出力する。　*
*    流用　　　　　　　　：　SFU3020L                          *
*    作成日／作成者　　　：　2019/01/18 INOUE                  *
*    更新日／更新者　　　：　                                  *
*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NKE0080L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/01/18.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     YA        IS   YA
     YB        IS   YB
     YA-22     IS   YA-22
     YB-22     IS   YB-22
     YB-21     IS   YB-21
     YA-21     IS   YA-21
     STATION   IS   STAT
     CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*出荷確定累積ファイル
     SELECT   RUISYKL2  ASSIGN    TO        DA-01-VI-RUISYKL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       RUI-F01
                                            RUI-F02
                                            RUI-F03
                                            RUI-F06
                                            RUI-F07
                                            RUI-F09
                                            RUI-F10
                                            RUI-F11
                        FILE  STATUS   IS   RUI-STATUS.
*倉庫マスタ
     SELECT     ZSOKMS1 ASSIGN    TO       DA-01-VI-ZSOKMS1
                        ORGANIZATION       INDEXED
                        ACCESS    MODE     RANDOM
                        RECORD    KEY      SOK-F01
                        FILE      STATUS   SOK-STATUS.
*取引先マスタ
     SELECT     TOKMS2  ASSIGN    TO       DA-01-VI-TOKMS2
                        ORGANIZATION       INDEXED
                        ACCESS    MODE     RANDOM
                        RECORD    KEY      TOK-F01
                        FILE      STATUS   TOK-STATUS.
*店舗マスタ
     SELECT     TENMS1  ASSIGN    TO       DA-01-VI-TENMS1
                        ORGANIZATION       INDEXED
                        ACCESS    MODE     RANDOM
                        RECORD    KEY      TEN-F52  TEN-F011
                        FILE      STATUS   TEN-STATUS.
*商品名称マスタ
     SELECT     MEIMS1  ASSIGN    TO       DA-01-VI-MEIMS1
                        ORGANIZATION       INDEXED
                        ACCESS    MODE     RANDOM
                        RECORD    KEY      MEI-F011  MEI-F0121
                                           MEI-F0122 MEI-F0123
                        FILE      STATUS   MEI-STATUS.
*担当者マスタ
     SELECT     TANMS1  ASSIGN    TO       DA-01-VI-TANMS1
                        ORGANIZATION       INDEXED
                        ACCESS    MODE     RANDOM
                        RECORD    KEY      TAN-F01  TAN-F02
                        FILE      STATUS   TAN-STATUS.
*プリンタ
     SELECT   PRTF      ASSIGN         LP-04-PRTF
                        FILE  STATUS   IS   PRT-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*出荷確定累積ファイル
 FD  RUISYKL2            LABEL RECORD   IS   STANDARD.
     COPY     RUISYKL2   OF        XFDLIB
     JOINING  RUI       PREFIX.
*倉庫マスタ
 FD  ZSOKMS1            LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS1   OF        XFDLIB
     JOINING  SOK       AS        PREFIX.
*取引先マスタ
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     TOKMS2    OF        XFDLIB
     JOINING  TOK       AS        PREFIX.
*店舗マスタ
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     TENMS1    OF        XFDLIB
     JOINING  TEN       AS        PREFIX.
*商品名称マスタ
 FD  MEIMS1             LABEL RECORD   IS   STANDARD.
     COPY     MEIMS1    OF        XFDLIB
     JOINING  MEI       AS        PREFIX.
*担当者マスタ
 FD  TANMS1             LABEL RECORD   IS   STANDARD.
     COPY     TANMS1    OF        XFDLIB
     JOINING  TAN       AS        PREFIX.
*プリンタ
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  SUTE-FLG                PIC  X(03)     VALUE  ZERO.
 01  PAGE-CNT                PIC  9(04)     VALUE  ZERO.
 01  LINE-CNT                PIC  9(02)     VALUE  ZERO.
 01  RUISYKL2-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  RUISYKL2-ERR-CNT        PIC  9(07)     VALUE  ZERO.
 01  PRINT-OUT-CNT           PIC  9(07)     VALUE  ZERO.
 01  ZSOKMS1-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  TOKMS2-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  TENMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  MEIMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  TANMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  WK-SUURYO               PIC S9(09)     VALUE  ZERO.
*取込日付／時刻バックアップ
 01  WK-KEY.
     03  WK-TRDATE           PIC  9(08)     VALUE  ZERO.
     03  WK-TRTIME           PIC  9(06)     VALUE  ZERO.
*システム日付の編集
 01  WK-SYS-DATE.
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*ステータス
 01  WK-ST.
     03  RUI-STATUS        PIC  X(02).
     03  SOK-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  TAN-STATUS        PIC  X(02).
     03  PRT-STATUS        PIC  X(02).
*BRK項目　エリア
     03  BRK-RUI-F03       PIC  9(06)    VALUE ZERO.
     03  BRK-RUI-F03-FLG   PIC  X(03)    VALUE SPACE.
     03  BRK-RUI-F06       PIC  9(08)    VALUE ZERO.
     03  BRK-RUI-F06-FLG   PIC  X(03)    VALUE SPACE.
*メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NKE0080L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NKE0080L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NKE0080L".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HD0.
     03  FILLER.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  X(08)     VALUE  "NKE0080L".
         05  FILLER          PIC  X(33)     VALUE  SPACE.
     03  FILLER         CHARACTER  TYPE YA-22.
         05  FILLER          PIC  N(11)     VALUE
         NC"＜出荷確定取込リスト＞".
     03  FILLER         CHARACTER  TYPE YA.
         05  FILLER          PIC  X(28)     VALUE  SPACE.
         05  HD0-YYYY        PIC  9(04).
         05  FILLER          PIC  N(01)     VALUE  NC"年".
         05  HD0-MM          PIC  Z9.
         05  FILLER          PIC  N(01)     VALUE  NC"月".
         05  HD0-DD          PIC  Z9.
         05  FILLER          PIC  N(01)     VALUE  NC"日".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  HD0-PCNT        PIC  ZZ9.
         05  FILLER          PIC  N(01)     VALUE  NC"頁".
 01  HD00.
     03  FILLER              PIC  X(50)     VALUE  SPACE.
     03  FILLER         CHARACTER  TYPE YA-22.
         05  HD00-CKUBUN     PIC  N(07).
     03  FILLER              PIC  X(38)     VALUE  SPACE.
     03  FILLER         CHARACTER  TYPE YA.
         05  HD00-HH         PIC  9(02).
         05  FILLER          PIC  N(01)     VALUE  NC"：".
         05  HD00-SS         PIC  9(02).
         05  FILLER          PIC  N(01)     VALUE  NC"：".
         05  HD00-MS         PIC  9(02).
 01  HD000.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(03)   VALUE  NC"倉庫：".
         05  HD000-SOKCD     PIC  X(02).
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  HD000-SOKNM     PIC  N(18).
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(05)   VALUE  NC"取込日付：".
         05  HD000-TDATE     PIC  X(10).
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(05)   VALUE  NC"取込時刻：".
         05  HD000-TTIME     PIC  X(08).
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(04)   VALUE  NC"担当者：".
         05  HD000-TRBUMON   PIC  X(04).
         05  FILLER          PIC  X(01)   VALUE  "-".
         05  HD000-TRTANTO   PIC  X(02).
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  HD000-TRTANNM   PIC  N(10).
*
 01  HD0000.
     03  FILLER         CHARACTER  TYPE YB.
         05  FILLER          PIC  X(33)     VALUE  SPACE.
         05  FILLER          PIC  N(05)     VALUE
             NC"エラー情報".
         05  FILLER          PIC  N(04)     VALUE
             NC"_：重複".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(05)     VALUE
             NC"_：売伝無".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(07)     VALUE
             NC"_：変ＴＢＬ無".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(05)     VALUE
             NC"_：名称無".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(09)     VALUE
             NC"_：出荷数＞発注数".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(07)     VALUE
             NC"_：出荷数＜０".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(05)     VALUE
             NC"_：基本無".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(05)     VALUE
             NC"_：予備８".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(05)     VALUE
             NC"_：予備９".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(06)     VALUE
             NC"_：予備１０".
*
 01  SEN.
     03  FILLER              PIC  X(136)    VALUE  ALL "-".
*
 01  HD01.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(05)   VALUE  NC"取引先情報".
         05  FILLER          PIC  X(104)  VALUE  SPACE.
         05  FILLER          PIC  N(10)   VALUE
                                       NC"＜　エラー区分　　＞".
*
 01  HD02.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(04)   VALUE  NC"店舗情報".
         05  FILLER          PIC  X(11)   VALUE  SPACE.
         05  FILLER          PIC  N(03)   VALUE  NC"納品日".
         05  FILLER          PIC  X(04)   VALUE  SPACE.
         05  FILLER          PIC  N(04)   VALUE  NC"伝票番号".
         05  FILLER          PIC  X(02)   VALUE  SPACE.
         05  FILLER          PIC  N(01)   VALUE  NC"行".
         05  FILLER          PIC  X(01)   VALUE  SPACE.
         05  FILLER          PIC  N(05)   VALUE  NC"ＪＡＮＣＤ".
         05  FILLER          PIC  X(04)   VALUE  SPACE.
         05  FILLER          PIC  N(03)   VALUE  NC"商品名".
         05  FILLER          PIC  X(25)   VALUE  SPACE.
         05  FILLER          PIC  N(03)   VALUE  NC"発注数".
         05  FILLER          PIC  X(05)   VALUE  SPACE.
         05  FILLER          PIC  N(03)   VALUE  NC"出荷数".
         05  FILLER          PIC  X(02)   VALUE  SPACE.
         05  FILLER          PIC  N(02)   VALUE  NC"棚番".
         05  FILLER          PIC  X(04)   VALUE  SPACE.
         05  FILLER          PIC  N(01)   VALUE  NC"_".
         05  FILLER          PIC  N(01)   VALUE  NC"_".
         05  FILLER          PIC  N(01)   VALUE  NC"_".
         05  FILLER          PIC  N(01)   VALUE  NC"_".
         05  FILLER          PIC  N(01)   VALUE  NC"_".
         05  FILLER          PIC  N(01)   VALUE  NC"_".
         05  FILLER          PIC  N(01)   VALUE  NC"_".
         05  FILLER          PIC  N(01)   VALUE  NC"_".
         05  FILLER          PIC  N(01)   VALUE  NC"_".
         05  FILLER          PIC  N(01)   VALUE  NC"_".


*
*01  HD03.
*    03  FILLER         CHARACTER TYPE YA.
*        05  FILLER          PIC  X(01)     VALUE  SPACE.
*        05  FILLER          PIC  N(02)     VALUE  NC"伝区".
*        05  FILLER          PIC  X(01)     VALUE  SPACE.
*        05  FILLER          PIC  N(02)     VALUE  NC"年度".
*        05  FILLER          PIC  X(04)     VALUE  SPACE.
*        05  FILLER          PIC  N(04)     VALUE  NC"シーズン".
*        05  FILLER          PIC  X(01)     VALUE  SPACE.
*        05  FILLER          PIC  N(02)     VALUE  NC"倉庫".
*        05  FILLER          PIC  X(03)     VALUE  SPACE.
*        05  FILLER          PIC  X(03)     VALUE  "(  ".
*        05  FILLER          PIC  N(07)
*                                 VALUE  NC"サカタ商品情報".
*        05  FILLER          PIC  X(04)     VALUE  "   )".
*        05  FILLER          PIC  X(01)     VALUE  SPACE.
*        05  FILLER          PIC  N(05)
*                                 VALUE  NC"ＪＡＮＣＤ".
*        05  FILLER          PIC  X(04)     VALUE  SPACE.
*        05  FILLER          PIC  N(03)     VALUE  NC"商品名".
*        05  FILLER          PIC  X(40)     VALUE  SPACE.
*        05  FILLER          PIC  N(12)     VALUE
*                            NC"＜　　エラー情報　　　＞".
*01  HD04.
*    03  FILLER         CHARACTER TYPE YB.
*        05  FILLER          PIC  X(06)     VALUE  SPACE.
*        05  FILLER          PIC  N(04)     VALUE  NC"入荷予定".
*        05  FILLER          PIC  X(02)     VALUE  SPACE.
*    03  FILLER         CHARACTER TYPE YA.
*        05  FILLER          PIC  N(03)     VALUE  NC"仕入先".
*        05  FILLER          PIC  X(03)     VALUE  SPACE.
*        05  FILLER          PIC  N(02)     VALUE  NC"棚番".
*        05  FILLER          PIC  X(07)     VALUE  SPACE.
*        05  FILLER          PIC  N(05)
*                                 VALUE  NC"発　注　数".
*        05  FILLER          PIC  X(22)     VALUE  SPACE.
*        05  FILLER          PIC  N(03)     VALUE  NC"ＭＳＧ".
*        05  FILLER          PIC  X(39)     VALUE  SPACE.
*    03  FILLER         CHARACTER TYPE YB.
*        05  FILLER          PIC  N(10)
*                            VALUE NC"__________".
*        05  FILLER          PIC  N(05)
*                            VALUE NC"_____".
*
 01  MS01.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-TOKCD      PIC  9(08).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-TOKNM      PIC  N(15).
*
 01  MS02.
     03  FILLER              CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS02-TENCD      PIC  9(05).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS02-TENNM      PIC  N(05).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS02-NDATE      PIC  X(10).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS02-DENNO      PIC  9(09).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS02-GYONO      PIC  Z9.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS02-JANCD      PIC  X(13).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS02-SYONAME    PIC  X(25).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS02-HACSU      PIC  --,---,--9.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS02-SYKSU      PIC  --,---,--9.
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS02-TANANO     PIC  X(06).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS02-ERR1       PIC  N(01).
         05  MS02-ERR2       PIC  N(01).
         05  MS02-ERR3       PIC  N(01).
         05  MS02-ERR4       PIC  N(01).
         05  MS02-ERR5       PIC  N(01).
         05  MS02-ERR6       PIC  N(01).
         05  MS02-ERR7       PIC  N(01).
         05  MS02-ERR8       PIC  N(01).
         05  MS02-ERR9       PIC  N(01).
         05  MS02-ERR10      PIC  N(01).
*
*対象データなし
 01  LST-DATA-X.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
             NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃".
 01  LST-DATA-Y.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
             NC"＃　　　　対象データは０件です　！！　　　＃".
 01  LST-DATA-Z.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
             NC"＃　　　　　　　　　　　　　　　　　　　　＃".
*
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
 01  P-LINE1            PIC  X(136)    VALUE  ALL   "-".
 01  P-LINE2            PIC  X(136)    VALUE  ALL   "=".
*時刻編集
 01  SYS-TIME                PIC  9(08).
 01  WK-TIME      REDEFINES  SYS-TIME.
   03  WK-TIME-HM            PIC  9(06).
   03  WK-TIME-FIL           PIC  X(02).
*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-IN-BUMON         PIC   X(04).
 01  PARA-IN-TANCD         PIC   X(02).
 01  PARA-IN-SOKCD         PIC   X(02).
 01  PARA-IN-CKUBUN        PIC   X(01).
 01  PARA-IN-TDATE         PIC   9(08).
 01  PARA-IN-TTIME         PIC   9(06).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION
                                 USING PARA-IN-BUMON
                                       PARA-IN-TANCD
                                       PARA-IN-SOKCD
                                       PARA-IN-CKUBUN
                                       PARA-IN-TDATE
                                       PARA-IN-TTIME.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   RUISYKL2.
     MOVE      "RUISYKL2"    TO   AB-FILE.
     MOVE      RUI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZSOKMS1.
     MOVE      "ZSOKMS1 "   TO   AB-FILE.
     MOVE      SOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TOKMS2.
     MOVE      "TOKMS2  "   TO   AB-FILE.
     MOVE      TOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TENMS1.
     MOVE      "TENMS1  "   TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   MEIMS1.
     MOVE      "MEIMS1  "   TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TANMS1.
     MOVE      "TANMS1  "   TO   AB-FILE.
     MOVE      TAN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   PRTF.
     MOVE      "PRTF    "   TO   AB-FILE.
     MOVE      PRT-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*ファイルＯＰＥＮ
     OPEN     INPUT     RUISYKL2
                        ZSOKMS1
                        TOKMS2
                        TENMS1
                        MEIMS1
                        TANMS1.
     OPEN     OUTPUT    PRTF.
*
     DISPLAY  MSG-START UPON CONS.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
     ACCEPT   SYS-TIME          FROM   TIME.
*出荷確定累積ファイルスタート
     PERFORM  RUISYKL2-START-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　対象データ　　なし　＃＃"  UPON CONS
          PERFORM  HEAD-WT-SEC
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  5
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Y  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  1
          MOVE    "END"         TO    END-FLG
          GO                    TO    INIT-EXIT
     END-IF.
*出荷確定累積ファイル読込
     PERFORM RUISYKL2-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　対象データ　　なし　＃＃"  UPON CONS
          PERFORM  HEAD-WT-SEC
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  5
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Y  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  1
          MOVE    "END"         TO    END-FLG
          GO                    TO    INIT-EXIT
     END-IF.
*
     MOVE    RUI-F03            TO    BRK-RUI-F03.
     MOVE    RUI-F06            TO    BRK-RUI-F06.
     MOVE    "STR"              TO    BRK-RUI-F06-FLG.
*
*ヘッダ印刷
     PERFORM HEAD-WT-SEC.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    出荷確定累積ファイルスタート
****************************************************************
 RUISYKL2-START-SEC          SECTION.
*
     MOVE    "RUISYKL2-START-SEC"  TO   S-NAME.
*
     MOVE     SPACE               TO   RUI-REC.
     INITIALIZE                        RUI-REC.
*
     MOVE     PARA-IN-SOKCD       TO   RUI-F01.
     MOVE     PARA-IN-TDATE       TO   RUI-F02.
     MOVE     PARA-IN-TTIME       TO   RUI-F03.
     MOVE     ZERO                TO   RUI-F06.
     MOVE     ZERO                TO   RUI-F07.
     MOVE     ZERO                TO   RUI-F09.
     MOVE     ZERO                TO   RUI-F10.
     MOVE     ZERO                TO   RUI-F11.
*
     START    RUISYKL2  KEY  IS  >=    RUI-F01 RUI-F02
                                       RUI-F03 RUI-F06
                                       RUI-F07 RUI-F09
                                       RUI-F10 RUI-F11
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 RUISYKL2-START-EXIT.
     EXIT.
*
****************************************************************
*    出荷確定累積ファイル読込
****************************************************************
 RUISYKL2-READ-SEC           SECTION.
*
     MOVE    "RUISYKL2-READ-SEC"   TO   S-NAME.
*
 RUISYKL2-READ-01.
     READ     RUISYKL2  AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   RUISYKL2-READ-EXIT
     END-READ.
*条件範囲内判定
     IF     ( RUI-F01 = PARA-IN-SOKCD ) AND
            ( RUI-F02 = PARA-IN-TDATE )
              IF    PARA-IN-TTIME = ZERO
                    CONTINUE
              ELSE
                    IF  RUI-F03 = PARA-IN-TTIME
                        CONTINUE
                    ELSE
                        MOVE   "END"  TO   END-FLG
                        GO            TO   RUISYKL2-READ-EXIT
                    END-IF
              END-IF
     ELSE
              MOVE     "END"      TO   END-FLG
              GO                  TO   RUISYKL2-READ-EXIT
     END-IF.
*エラー件数カウント
     IF       RUI-F89  =  "1"
              ADD          1      TO   RUISYKL2-ERR-CNT
     END-IF.
*件数カウント
     ADD      1                   TO   RUISYKL2-READ-CNT.
*
*帳票区分別制御
     IF       PARA-IN-CKUBUN = " "
          IF  RUI-F89  =  "1"
              CONTINUE
          ELSE
              GO     TO    RUISYKL2-READ-01
     END-IF.
*
 RUISYKL2-READ-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
 MAIN-010.
*印刷処理
     PERFORM  MEISAI-WT-SEC.
     MOVE     SPACE  TO   PRT-REC.
*
 MAIN-020.
     PERFORM  RUISYKL2-READ-SEC.
*
 MAIN-030.
     IF       RUI-F03  NOT = BRK-RUI-F03
              MOVE     "BRK"     TO   BRK-RUI-F03-FLG
     END-IF.
*
 MAIN-040.
     IF       RUI-F06  NOT = BRK-RUI-F06
              MOVE     "BRK"     TO   BRK-RUI-F06-FLG
     END-IF.
*
 MAIN-EXIT.
     EXIT.
*
*--------------------------------------------------------------*
*    ヘッダ印字
*--------------------------------------------------------------*
 HEAD-WT-SEC            SECTION.
     MOVE    "HEAD-WT-SEC"        TO   S-NAME.
*    改頁判定
     IF       PAGE-CNT  >   ZERO
              MOVE      SPACE     TO   PRT-REC
              WRITE     PRT-REC   AFTER   PAGE
     END-IF.
*    行カウンター初期化
     MOVE     ZERO                TO   LINE-CNT.
*    頁カウンター
     ADD      1                   TO   PAGE-CNT.
     MOVE     PAGE-CNT            TO   HD0-PCNT.
*    システム日付セット
     MOVE     SYS-DATEW(1:4)      TO   HD0-YYYY.
     MOVE     SYS-DATEW(5:2)      TO   HD0-MM.
     MOVE     SYS-DATEW(7:2)      TO   HD0-DD.
*    システム時刻セット
     MOVE     WK-TIME-HM(1:2)     TO   HD00-HH.
     MOVE     WK-TIME-HM(3:2)     TO   HD00-SS.
     MOVE     WK-TIME-HM(5:2)     TO   HD00-MS.
*    帳票区分セット
     EVALUATE  PARA-IN-CKUBUN
       WHEN "1"   MOVE  NC"（　全　件　）" TO   HD00-CKUBUN
       WHEN " "   MOVE  NC"（エラーのみ）" TO   HD00-CKUBUN
       WHEN OTHER MOVE  NC"（？？？？？）" TO   HD00-CKUBUN
     END-EVALUATE.
*    倉庫名取得
     MOVE     PARA-IN-SOKCD       TO   SOK-F01 HD000-SOKCD.
     PERFORM  ZSOKMS1-READ-SEC.
     IF       ZSOKMS1-INV-FLG = SPACE
              MOVE  SOK-F02       TO   HD000-SOKNM
     ELSE
              MOVE  ALL NC"？"    TO   HD000-SOKNM
     END-IF.
*    取込日付セット
     MOVE     PARA-IN-TDATE(1:4)  TO   HD000-TDATE(1:4).
     MOVE     "/"                 TO   HD000-TDATE(5:1).
     MOVE     PARA-IN-TDATE(5:2)  TO   HD000-TDATE(6:2).
     MOVE     "/"                 TO   HD000-TDATE(8:1).
     MOVE     PARA-IN-TDATE(7:2)  TO   HD000-TDATE(9:2).
*    取込時刻セット
     MOVE     RUI-F03(1:2)        TO   HD000-TTIME(1:2).
     MOVE     ":"                 TO   HD000-TTIME(3:1).
     MOVE     RUI-F03(3:2)        TO   HD000-TTIME(4:2).
     MOVE     ":"                 TO   HD000-TTIME(6:1).
     MOVE     RUI-F03(5:2)        TO   HD000-TTIME(7:2).
*    担当者名取得
     MOVE     PARA-IN-BUMON       TO   TAN-F01 HD000-TRBUMON.
     MOVE     PARA-IN-TANCD       TO   TAN-F02 HD000-TRTANTO.
     PERFORM  TANMS1-READ-SEC.
     IF       TANMS1-INV-FLG = SPACE
              MOVE  TAN-F03       TO   HD000-TRTANNM
     ELSE
              MOVE  ALL NC"？"    TO   HD000-TRTANNM
     END-IF.
*    ヘッダ印刷
     WRITE    PRT-REC       FROM  HD0     AFTER  2.
     WRITE    PRT-REC       FROM  HD00    AFTER  2.
     WRITE    PRT-REC       FROM  HD000   AFTER  1.
     WRITE    PRT-REC       FROM  HD0000  AFTER  1.
     WRITE    PRT-REC       FROM  SEN     AFTER  1.
     WRITE    PRT-REC       FROM  HD01    AFTER  1.
     WRITE    PRT-REC       FROM  HD02    AFTER  1.
     WRITE    PRT-REC       FROM  SEN     AFTER  1.
     MOVE     SPACE               TO      PRT-REC.
     WRITE    PRT-REC                     AFTER  1.
*行カウントアップ
     MOVE     11                  TO      LINE-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細印字
*--------------------------------------------------------------*
 MEISAI-WT-SEC          SECTION.
     MOVE    "MEISAI-WT-SEC"      TO   S-NAME.
*
*   ブレイク改頁判定
     IF       BRK-RUI-F03-FLG     =   "BRK"
              PERFORM  HEAD-WT-SEC
              MOVE     RUI-F03    TO   BRK-RUI-F03
              MOVE     RUI-F06    TO   BRK-RUI-F06
              MOVE     "   "      TO   BRK-RUI-F03-FLG
              MOVE     "STR"      TO   BRK-RUI-F06-FLG
     END-IF.
*   改頁判定
     IF       LINE-CNT >  50
              PERFORM  HEAD-WT-SEC
              MOVE    "STR"                TO   BRK-RUI-F06-FLG
     END-IF.
*
*明細１行目(取引先行)
     IF       BRK-RUI-F06-FLG     =   "STR"
*             取引先行
              MOVE     RUI-F06             TO   TOK-F01
                                                MS01-TOKCD
              PERFORM  TOKMS2-READ-SEC
              IF       TOKMS2-INV-FLG = SPACE
                       MOVE  TOK-F02       TO   MS01-TOKNM
              ELSE
                       MOVE  ALL NC"？"    TO   MS01-TOKNM
              END-IF
*             取引先行印刷
              WRITE    PRT-REC     FROM  MS01   AFTER  1
*             行カウント
              ADD      1                   TO   LINE-CNT
              MOVE     "   "               TO   BRK-RUI-F06-FLG
     END-IF.
*
*
     IF       BRK-RUI-F06-FLG     =   "BRK"
*             線行印刷
              WRITE    PRT-REC     FROM  SEN    AFTER  2
*             行カウント
              ADD      2                   TO   LINE-CNT
*             取引先行
              MOVE     RUI-F06             TO   TOK-F01
                                                MS01-TOKCD
              PERFORM  TOKMS2-READ-SEC
              IF       TOKMS2-INV-FLG = SPACE
                       MOVE  TOK-F02       TO   MS01-TOKNM
              ELSE
                       MOVE  ALL NC"？"    TO   MS01-TOKNM
              END-IF
*             取引先行印刷
              WRITE    PRT-REC     FROM  MS01   AFTER  2
*             行カウント
              ADD      2                   TO   LINE-CNT
*             ブレイクリセット
              MOVE     RUI-F06             TO   BRK-RUI-F06
              MOVE     "   "               TO   BRK-RUI-F06-FLG
     END-IF.
*
*---------------------------------------------------
*明細２行目(内容）
*   店舗情報
     MOVE     RUI-F06             TO   TEN-F52
     MOVE     RUI-F07             TO   TEN-F011
                                       MS02-TENCD
     PERFORM  TENMS1-READ-SEC
     IF       TENMS1-INV-FLG = SPACE
              MOVE  TEN-F03       TO   MS02-TENNM
     ELSE
              MOVE  ALL NC"？"    TO   MS02-TENNM
     END-IF
*   納品日
     MOVE     RUI-F09(1:4)        TO   MS02-NDATE(1:4).
     MOVE     "/"                 TO   MS02-NDATE(5:1).
     MOVE     RUI-F09(5:2)        TO   MS02-NDATE(6:2).
     MOVE     "/"                 TO   MS02-NDATE(8:1).
     MOVE     RUI-F09(7:2)        TO   MS02-NDATE(9:2).
*   伝票番号
     MOVE     RUI-F10             TO   MS02-DENNO.
*   行番号
     MOVE     RUI-F11             TO   MS02-GYONO.
*   ＪＡＮＣＤ
     MOVE     RUI-F12             TO   MS02-JANCD.
*   商品名
     MOVE     RUI-F16             TO   MEI-F011.
     MOVE     RUI-F17(1:5)        TO   MEI-F0121.
     MOVE     RUI-F17(6:2)        TO   MEI-F0122.
     MOVE     RUI-F17(8:1)        TO   MEI-F0123.
     PERFORM  MEIMS1-READ-SEC
     IF       MEIMS1-INV-FLG = SPACE
              MOVE  MEI-F03       TO   MS02-SYONAME
     ELSE
              MOVE  ALL "?"       TO   MS02-SYONAME
     END-IF
*
*   発注数
     MOVE     RUI-F13             TO   MS02-HACSU.
*   出荷数
     MOVE     RUI-F14             TO   MS02-SYKSU.
*   棚番
     MOVE     RUI-F15             TO   MS02-TANANO.
*   エラー区分１
     IF  RUI-F90  = " "
              MOVE SPACE          TO   MS02-ERR1
     ELSE
              MOVE NC"×"         TO   MS02-ERR1
     END-IF.
*    エラー区分２
     IF  RUI-F91  = " "
              MOVE SPACE          TO   MS02-ERR2
     ELSE
              MOVE NC"×"         TO   MS02-ERR2
     END-IF.
*    エラー区分３
     IF  RUI-F92  = " "
              MOVE SPACE          TO   MS02-ERR3
     ELSE
              MOVE NC"×"         TO   MS02-ERR3
     END-IF.
*    エラー区分４
     IF  RUI-F93  = " "
              MOVE SPACE          TO   MS02-ERR4
     ELSE
              MOVE NC"×"         TO   MS02-ERR4
     END-IF.
*    エラー区分５
     IF  RUI-F94  = " "
              MOVE SPACE          TO   MS02-ERR5
     ELSE
              MOVE NC"×"         TO   MS02-ERR5
     END-IF.
*    エラー区分６
     IF  RUI-F95  = " "
              MOVE SPACE          TO   MS02-ERR6
     ELSE
              MOVE NC"×"         TO   MS02-ERR6
     END-IF.
*    エラー区分７
     IF  RUI-F96  = " "
              MOVE SPACE          TO   MS02-ERR7
     ELSE
              MOVE NC"×"         TO   MS02-ERR7
     END-IF.
*    エラー区分８
     IF  RUI-F97  = " "
              MOVE SPACE          TO   MS02-ERR8
     ELSE
              MOVE NC"×"         TO   MS02-ERR8
     END-IF.
*    エラー区分９
     IF  RUI-F98  = " "
              MOVE SPACE          TO   MS02-ERR9
     ELSE
              MOVE NC"×"         TO   MS02-ERR9
     END-IF.
*    エラー区分１０
     IF  RUI-F99  = " "
              MOVE SPACE          TO   MS02-ERR10
     ELSE
              MOVE NC"×"         TO   MS02-ERR10
     END-IF.
*
*    明細印刷
     WRITE    PRT-REC       FROM  MS02 AFTER  1.
     ADD      1                   TO   PRINT-OUT-CNT.
*    WRITE    PRT-REC       FROM  SEN  AFTER  1.
*行カウント
*    ADD      2                   TO   LINE-CNT.
     ADD      1                   TO   LINE-CNT.
*
 MEISAI-WT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*件数表示
*  ＝出荷確定累積ファイル読込件数
     DISPLAY "RUISYKL2 READ-CNT  = " RUISYKL2-READ-CNT UPON CONS.
     DISPLAY "OUTPUT   PRINT-CNT = " PRINT-OUT-CNT UPON CONS.
     DISPLAY "         ERR-CNT   = " RUISYKL2-ERR-CNT UPON CONS.
     DISPLAY "         PAGE-CNT  = " PAGE-CNT UPON CONS.
*
     CLOSE     RUISYKL2  ZSOKMS1  TOKMS2  TENMS1
               MEIMS1    TANMS1   PRTF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*    倉庫マスタ索引
****************************************************************
 ZSOKMS1-READ-SEC           SECTION.
     MOVE "ZSOKMS1-READ-SEC"      TO   S-NAME.
*
     READ       ZSOKMS1
         INVALID       MOVE "INV" TO   ZSOKMS1-INV-FLG
         NOT  INVALID  MOVE SPACE TO   ZSOKMS1-INV-FLG
     END-READ.
*
 ZSOKMS1-READ-EXIT.
     EXIT.
****************************************************************
*    取引先マスタ索引
****************************************************************
 TOKMS2-READ-SEC           SECTION.
     MOVE "TOKMS2-READ-SEC"       TO   S-NAME.
*
     READ       TOKMS2
         INVALID       MOVE "INV" TO   TOKMS2-INV-FLG
         NOT  INVALID  MOVE SPACE TO   TOKMS2-INV-FLG
     END-READ.
*
 TOKMS2-READ-EXIT.
     EXIT.
****************************************************************
*    店舗マスタ索引
****************************************************************
 TENMS1-READ-SEC           SECTION.
     MOVE "TENMS1-READ-SEC"       TO   S-NAME.
*
     READ       TENMS1
         INVALID       MOVE "INV" TO   TENMS1-INV-FLG
         NOT  INVALID  MOVE SPACE TO   TENMS1-INV-FLG
     END-READ.
*
 TENMS1-READ-EXIT.
     EXIT.
****************************************************************
*    商品名称マスタ索引
****************************************************************
 MEIMS1-READ-SEC           SECTION.
     MOVE "MEIMS1-READ-SEC"       TO   S-NAME.
*
     READ       MEIMS1
         INVALID       MOVE "INV" TO   MEIMS1-INV-FLG
         NOT  INVALID  MOVE SPACE TO   MEIMS1-INV-FLG
     END-READ.
*
 MEIMS1-READ-EXIT.
     EXIT.
****************************************************************
*    担当者マスタ索引
****************************************************************
 TANMS1-READ-SEC           SECTION.
     MOVE "TANMS1-READ-SEC"       TO   S-NAME.
*
     READ       TANMS1
         INVALID       MOVE "INV" TO   TANMS1-INV-FLG
         NOT  INVALID  MOVE SPACE TO   TANMS1-INV-FLG
     END-READ.
*
 TANMS1-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

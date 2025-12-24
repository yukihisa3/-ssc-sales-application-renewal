# NKE1060L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NKE1060L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷検品　　　　　　　            *
*    モジュール名　　　　：　出荷確定取込リスト発行　　　　　　*
*    処理概要　　　　　　：　検品システムより受け取ったデータの*
*                            整合性チェック結果をリスト確認、　*
*                            または全件出力。　　　　　　　　　*
*    流用　　　　　　　　：　NKE0080L                          *
*    作成日／作成者　　　：　2019/06/28 NAV                    *
*    更新日／更新者　　　：　                                  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NKE1060L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/06/28.
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
*検品結果集計ファイル
     SELECT   CZSUMXX2  ASSIGN    TO        DA-01-VI-CZSUMXX2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       SUM-F04
                                            SUM-F94
                                            SUM-F95
                                            SUM-F03
                                            SUM-F08
                                            SUM-F07
                                            SUM-F10
                                            SUM-F11
                        FILE  STATUS   IS   SUM-STATUS.
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
*検品結果集計ファイル
 FD  CZSUMXX2            LABEL RECORD   IS   STANDARD.
     COPY     CZSUML2    OF        XFDLIB
     JOINING  SUM       PREFIX.
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
 01  CZSUMXX2-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  CZSUMXX2-ERR-CNT        PIC  9(07)     VALUE  ZERO.
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
     03  SUM-STATUS        PIC  X(02).
     03  SOK-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  TAN-STATUS        PIC  X(02).
     03  PRT-STATUS        PIC  X(02).
*BRK項目　エリア
     03  BRK-SUM-F95       PIC  9(06)    VALUE ZERO.
     03  BRK-SUM-F95-FLG   PIC  X(03)    VALUE SPACE.
     03  BRK-SUM-F03       PIC  9(08)    VALUE ZERO.
     03  BRK-SUM-F03-FLG   PIC  X(03)    VALUE SPACE.
*メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NKE1060L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NKE1060L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NKE1060L".
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
         05  FILLER          PIC  X(08)     VALUE  "NKE1060L".
         05  FILLER          PIC  X(23)     VALUE  SPACE.
     03  FILLER         CHARACTER  TYPE YA-22.
         05  FILLER          PIC  N(15)     VALUE
         NC"＜カインズ出荷確定取込リスト＞".
     03  FILLER         CHARACTER  TYPE YA.
         05  FILLER          PIC  X(22)     VALUE  SPACE.
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
         05  FILLER          PIC  X(27)     VALUE  SPACE.
         05  FILLER          PIC  N(05)     VALUE
             NC"エラー情報".
         05  FILLER          PIC  N(06)     VALUE
             NC"_：売伝Ｆ無".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(06)     VALUE
             NC"_：在庫Ｍ無".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(06)     VALUE
             NC"_：発注Ｄ無".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(10)     VALUE
             NC"_：出荷数＜＞発注数".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(07)     VALUE
             NC"_：出荷数≦０".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(06)     VALUE
             NC"_：予備６".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(05)     VALUE
             NC"_：予備７".
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
                        PROCEDURE   CZSUMXX2.
     MOVE      "CZSUMXX2"    TO   AB-FILE.
     MOVE      SUM-STATUS   TO   AB-STS.
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
     OPEN     INPUT     CZSUMXX2
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
*検品結果集計ファイルスタート
     PERFORM  CZSUMXX2-START-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　対象データ　　なし．＃＃"  UPON CONS
          PERFORM  HEAD-WT-SEC
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  5
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Y  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  1
          MOVE    "END"         TO    END-FLG
          GO                    TO    INIT-EXIT
     END-IF.
*検品結果集計ファイル読込
     PERFORM CZSUMXX2-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　対象データ　　なし。＃＃"  UPON CONS
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
     MOVE    SUM-F95            TO    BRK-SUM-F95.
     MOVE    SUM-F03            TO    BRK-SUM-F03.
     MOVE    "STR"              TO    BRK-SUM-F03-FLG.
*
*ヘッダ印刷
     PERFORM HEAD-WT-SEC.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    検品結果集計ファイルスタート
****************************************************************
 CZSUMXX2-START-SEC          SECTION.
*
     MOVE    "CZSUMXX2-START-SEC"  TO   S-NAME.
*
     MOVE     SPACE               TO   SUM-REC.
     INITIALIZE                        SUM-REC.
*
     MOVE     PARA-IN-SOKCD       TO   SUM-F04.
*    MOVE     PARA-IN-TDATE       TO   SUM-F94.
*    MOVE     PARA-IN-TTIME       TO   SUM-F95.
     MOVE     ZERO                TO   SUM-F94.
     MOVE     ZERO                TO   SUM-F95.
     MOVE     ZERO                TO   SUM-F03.
     MOVE     ZERO                TO   SUM-F08.
     MOVE     ZERO                TO   SUM-F07.
     MOVE     ZERO                TO   SUM-F10.
     MOVE     ZERO                TO   SUM-F11.
*
     START    CZSUMXX2  KEY  IS  >=    SUM-F04 SUM-F94
                                       SUM-F95 SUM-F03
                                       SUM-F08 SUM-F07
                                       SUM-F10 SUM-F11
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 CZSUMXX2-START-EXIT.
     EXIT.
*
****************************************************************
*    検品結果集計ファイル読込
****************************************************************
 CZSUMXX2-READ-SEC           SECTION.
*
     MOVE    "CZSUMXX2-READ-SEC"   TO   S-NAME.
*
 CZSUMXX2-READ-01.
     READ     CZSUMXX2  AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   CZSUMXX2-READ-EXIT
     END-READ.
*条件範囲内判定
     IF       SUM-F04 = PARA-IN-SOKCD
*             IF  ( PARA-IN-TDATE  =  ZERO ) AND
*                 ( PARA-IN-TTIME  =  ZERO )
                    CONTINUE
*             ELSE
*                   IF  SUM-F95 = PARA-IN-TTIME
*                       CONTINUE
*                   ELSE
*                       MOVE   "END"  TO   END-FLG
*                       GO            TO   CZSUMXX2-READ-EXIT
*                   END-IF
*             END-IF
     ELSE
              MOVE     "END"      TO   END-FLG
              GO                  TO   CZSUMXX2-READ-EXIT
     END-IF.
*エラー件数カウント
     IF       SUM-F17  =  "1"
              ADD          1      TO   CZSUMXX2-ERR-CNT
     END-IF.
*件数カウント
     ADD      1                   TO   CZSUMXX2-READ-CNT.
*
*帳票区分別制御
     IF       PARA-IN-CKUBUN = " "
          IF  SUM-F17  =  "1"
              CONTINUE
          ELSE
              GO     TO    CZSUMXX2-READ-01
          END-IF
     END-IF.
*
 CZSUMXX2-READ-EXIT.
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
     PERFORM  CZSUMXX2-READ-SEC.
*
 MAIN-030.
     IF       SUM-F95  NOT = BRK-SUM-F95
              MOVE     "BRK"     TO   BRK-SUM-F95-FLG
     END-IF.
*
 MAIN-040.
     IF       SUM-F03  NOT = BRK-SUM-F03
              MOVE     "BRK"     TO   BRK-SUM-F03-FLG
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
     MOVE     SUM-F95(1:2)        TO   HD000-TTIME(1:2).
     MOVE     ":"                 TO   HD000-TTIME(3:1).
     MOVE     SUM-F95(3:2)        TO   HD000-TTIME(4:2).
     MOVE     ":"                 TO   HD000-TTIME(6:1).
     MOVE     SUM-F95(5:2)        TO   HD000-TTIME(7:2).
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
     IF       BRK-SUM-F95-FLG     =   "BRK"
              PERFORM  HEAD-WT-SEC
              MOVE     SUM-F95    TO   BRK-SUM-F95
              MOVE     SUM-F03    TO   BRK-SUM-F03
              MOVE     "   "      TO   BRK-SUM-F95-FLG
              MOVE     "STR"      TO   BRK-SUM-F03-FLG
     END-IF.
*   改頁判定
     IF       LINE-CNT >  50
              PERFORM  HEAD-WT-SEC
              MOVE    "STR"                TO   BRK-SUM-F03-FLG
     END-IF.
*
*明細１行目(取引先行)
     IF       BRK-SUM-F03-FLG     =   "STR"
*             取引先行
              MOVE     SUM-F03             TO   TOK-F01
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
              MOVE     "   "               TO   BRK-SUM-F03-FLG
     END-IF.
*
*
     IF       BRK-SUM-F03-FLG     =   "BRK"
*             線行印刷
              WRITE    PRT-REC     FROM  SEN    AFTER  2
*             行カウント
              ADD      2                   TO   LINE-CNT
*             取引先行
              MOVE     SUM-F03             TO   TOK-F01
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
              MOVE     SUM-F03             TO   BRK-SUM-F03
              MOVE     "   "               TO   BRK-SUM-F03-FLG
     END-IF.
*
*---------------------------------------------------
*明細２行目(内容）
*   店舗情報
     MOVE     SUM-F03             TO   TEN-F52
     MOVE     SUM-F08             TO   TEN-F011
                                       MS02-TENCD
     PERFORM  TENMS1-READ-SEC
     IF       TENMS1-INV-FLG = SPACE
              MOVE  TEN-F03       TO   MS02-TENNM
     ELSE
              MOVE  ALL NC"？"    TO   MS02-TENNM
     END-IF
*   納品日
     MOVE     SUM-F07(1:4)        TO   MS02-NDATE(1:4).
     MOVE     "/"                 TO   MS02-NDATE(5:1).
     MOVE     SUM-F07(5:2)        TO   MS02-NDATE(6:2).
     MOVE     "/"                 TO   MS02-NDATE(8:1).
     MOVE     SUM-F07(7:2)        TO   MS02-NDATE(9:2).
*   伝票番号
     MOVE     SUM-F10             TO   MS02-DENNO.
*   行番号
     MOVE     SUM-F11             TO   MS02-GYONO.
*   ＪＡＮＣＤ
     MOVE     SUM-F13             TO   MS02-JANCD.
*   商品名
     MOVE     SUM-F12(1:8)        TO   MEI-F011.
     MOVE     SUM-F12(9:5)        TO   MEI-F0121.
     MOVE     SUM-F12(14:2)       TO   MEI-F0122.
     MOVE     SUM-F12(16:1)       TO   MEI-F0123.
     PERFORM  MEIMS1-READ-SEC
     IF       MEIMS1-INV-FLG = SPACE
              MOVE  MEI-F03       TO   MS02-SYONAME
     ELSE
              MOVE  ALL "?"       TO   MS02-SYONAME
     END-IF
*
*   発注数
     MOVE     SUM-F15             TO   MS02-HACSU.
*   検品数
     MOVE     SUM-F16             TO   MS02-SYKSU.
*   棚番
     MOVE     SUM-F14             TO   MS02-TANANO.
*   エラー区分１
     IF  SUM-F18  = " "
              MOVE SPACE          TO   MS02-ERR1
     ELSE
              MOVE NC"×"         TO   MS02-ERR1
     END-IF.
*    エラー区分２
     IF  SUM-F19  = " "
              MOVE SPACE          TO   MS02-ERR2
     ELSE
              MOVE NC"×"         TO   MS02-ERR2
     END-IF.
*    エラー区分３
     IF  SUM-F20  = " "
              MOVE SPACE          TO   MS02-ERR3
     ELSE
              MOVE NC"×"         TO   MS02-ERR3
     END-IF.
*    エラー区分４
     IF  SUM-F21  = " "
              MOVE SPACE          TO   MS02-ERR4
     ELSE
              MOVE NC"×"         TO   MS02-ERR4
     END-IF.
*    エラー区分５
     IF  SUM-F22  = " "
              MOVE SPACE          TO   MS02-ERR5
     ELSE
              MOVE NC"×"         TO   MS02-ERR5
     END-IF.
*    エラー区分６
     IF  SUM-F23  = " "
              MOVE SPACE          TO   MS02-ERR6
     ELSE
              MOVE NC"×"         TO   MS02-ERR6
     END-IF.
*    エラー区分７
     IF  SUM-F24  = " "
              MOVE SPACE          TO   MS02-ERR7
     ELSE
              MOVE NC"×"         TO   MS02-ERR7
     END-IF.
*    エラー区分８
     IF  SUM-F25  = " "
              MOVE SPACE          TO   MS02-ERR8
     ELSE
              MOVE NC"×"         TO   MS02-ERR8
     END-IF.
*    エラー区分９
     IF  SUM-F26  = " "
              MOVE SPACE          TO   MS02-ERR9
     ELSE
              MOVE NC"×"         TO   MS02-ERR9
     END-IF.
*    エラー区分１０
     IF  SUM-F27  = " "
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
*  ＝検品結果集計ファイル読込件数
     DISPLAY "CZSUMXX2 READ-CNT  = " CZSUMXX2-READ-CNT UPON CONS.
     DISPLAY "OUTPUT   PRINT-CNT = " PRINT-OUT-CNT UPON CONS.
     DISPLAY "         ERR-CNT   = " CZSUMXX2-ERR-CNT UPON CONS.
     DISPLAY "         PAGE-CNT  = " PAGE-CNT UPON CONS.
*
     CLOSE     CZSUMXX2  ZSOKMS1  TOKMS2  TENMS1
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

```

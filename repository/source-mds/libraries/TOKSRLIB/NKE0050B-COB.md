# NKE0050B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NKE0050B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷検品サブシステム              *
*    業務名　　　　　　　：　出荷検品                          *
*    モジュール名　　　　：　出荷データ抽出（手書）            *
*    作成日／更新日　　　：　2018/12/10                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　売上伝票Ｆを読み、条件Ｆの出力Ｆ番*
*                            号のファイルへ出荷検品用データを抽*
*                            出する。                          *
*    更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：　                                  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NKE0050B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2018/12/10.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     YA       IS        YA
     YB-21    IS        YB-21
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*売上伝票データ
     SELECT   SHTDENF   ASSIGN    TO        DA-01-VI-SHTDENL6
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F277  DEN-F274
                                            DEN-F09   DEN-F02
                                            DEN-F04   DEN-F051
                                            DEN-F07   DEN-F112
                                            DEN-F03
                        FILE      STATUS    IS   DEN-STATUS.
*条件ファイル
     SELECT   JYOKENF   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01
                                            JYO-F02
                        FILE  STATUS   IS   JYO-STATUS.
*商品変換テーブル
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE      STATUS    IS   TBL-STATUS.
*手書出荷指示
     SELECT   SNDTEDF   ASSIGN    TO        SNDTEDF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   D01-STATUS.
*件数ファイル
     SELECT   SNDSKKF   ASSIGN    TO        SNDSKKF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   K01-STATUS.
*プリント Ｆ
     SELECT      PRINTF      ASSIGN    TO        LP-04-PRTF.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENF
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    条件ファイル
******************************************************************
 FD  JYOKENF
                        LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO  AS   PREFIX.
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  SHOTBL1            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
*
******************************************************************
*    送信用店舗データ１
******************************************************************
 FD  SNDTEDF            BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
     COPY     SNDTEDF   OF        XFDLIB
              JOINING   D01       PREFIX.
******************************************************************
*    件数データ
******************************************************************
 FD  SNDSKKF            BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  K01-REC.
     03  K01-F01             PIC  9(08).
     03  K01-F02             PIC  X(02).
****************************************************************
*    FILE = プリント　ファイル                                 *
****************************************************************
 FD  PRINTF.
 01  PRINT-REC                    PIC       X(200).
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  KENPF-FLG               PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  IX                      PIC  9(02)     VALUE  ZERO.
 01  WK-SOKCD                PIC  X(02)     VALUE  SPACE.
 01  SKIP-FLG                PIC  9(01)     VALUE  ZERO.
 01  WK-DENPYO               PIC  9(09)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE.
         05  SYS-YY        PIC 9(02).
         05  SYS-MM        PIC 9(02).
         05  SYS-DD        PIC 9(02).
     03  SYS-DATEW.
         05  SYS-YY-W      PIC 9(04).
         05  SYS-MM-W      PIC 9(02).
         05  SYS-DD-W      PIC 9(02).
 01  SYS-TIME.
     03  SYS-HH              PIC  9(02).
     03  SYS-MN              PIC  9(02).
     03  SYS-SS              PIC  9(02).
     03  FILLER              PIC  9(02).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  D01-STATUS        PIC  X(02).
     03  K01-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NKE0050B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NKE0050B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NKE0050B".
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
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  WK-FLCD.
         05  WK-FLCD1        PIC  9(05)  VALUE  ZERO.
     03  WK-FLCDR  REDEFINES WK-FLCD.
         05  WK-FLCDT         PIC  9(05).
     03  WK-RTCD.
         05  WK-RTCD1        PIC  X(02)  VALUE  ZERO.
     03  WK-RTCDR  REDEFINES WK-RTCD.
         05  WK-RTCDT        PIC  X(02).
     03  WK-RTNM.
         05  WK-RTNM1        PIC  N(10)  VALUE  SPACE.
     03  WK-RTNMR  REDEFINES WK-RTNM.
         05  WK-RTNMT        PIC  N(10).
     03  WK-RTCNT.
         05  WK-RTCNT1       PIC  9(07)  VALUE  ZERO.
     03  WK-RTCNTR REDEFINES WK-RTCNT.
         05  WK-RTCNTT       PIC  9(07).
*    見出し行１
 01  MIDASHI1.
     03  FILLER                   PIC       X(37)  VALUE SPACE.
     03  FILLER                   PIC       N(18)  VALUE
       NC"【　出荷検品手伝票振分件数リスト　】"
       CHARACTER   TYPE   IS   YB-21.
     03  FILLER                   PIC       X(17)  VALUE SPACE.
     03  FILLER                   PIC       X(05)  VALUE
         "DATE:".
     03  YY                       PIC       99.
     03  FILLER                   PIC       X(01)  VALUE
         ".".
     03  MM                       PIC       Z9.
     03  FILLER                   PIC       X(01)  VALUE
         ".".
     03  DD                       PIC       Z9.
     03  FILLER                   PIC       X(01)  VALUE SPACE.
     03  FILLER                   PIC       X(05)  VALUE
         "PAGE:".
     03  PEIJI                    PIC       ZZZ9.
*    見出し行２
 01  MIDASHI2           CHARACTER TYPE      IS     YA.
     03  FILLER                   PIC       X(40)  VALUE SPACE.
     03  FILLER                   PIC       X(08)  VALUE
         "FILE-NO.".
     03  FILLER                   PIC       X(02)  VALUE SPACE.
     03  FILLER                   PIC       N(04)  VALUE
       NC"場所ＣＤ".
     03  FILLER                   PIC       X(02)  VALUE SPACE.
     03  FILLER                   PIC       N(05)  VALUE
       NC"出荷場所名".
     03  FILLER                   PIC       X(12)  VALUE SPACE.
     03  FILLER                   PIC       N(05)  VALUE
       NC"データ件数".
     03  FILLER                   PIC       X(78)  VALUE SPACE.
*    明細行
 01  MEISAI             CHARACTER TYPE      IS     YA.
     03  FILLER                   PIC       X(41)  VALUE SPACE.
     03  FILECD                   PIC       ZZ,ZZ9.
     03  FILLER                   PIC       X(08)  VALUE SPACE.
*****03  ROUTECD                  PIC       9(02).
     03  ROUTECD                  PIC       X(02).
     03  FILLER                   PIC       X(04)  VALUE SPACE.
     03  ROUTENM                  PIC       N(10).
     03  FILLER                   PIC       X(04)  VALUE SPACE.
     03  DATASU                   PIC       ZZ,ZZ9.
     03  FILLER                   PIC       X(78)  VALUE SPACE.
*    コメント行
 01  KOMENTO            CHARACTER TYPE      IS     YA.
     03  FILLER                   PIC       X(30)  VALUE SPACE.
     03  FILLER                   PIC       N(11)  VALUE
         NC"今回のバッチ番号は、［".
     03  FILLER                   PIC       X(01)  VALUE SPACE.
     03  B-YY                     PIC       9(04).
     03  FILLER                   PIC       X(01)  VALUE "/".
     03  B-MM                     PIC       9(02).
     03  FILLER                   PIC       X(01)  VALUE "/".
     03  B-DD                     PIC       9(02).
     03  FILLER                   PIC       X(04)  VALUE SPACE.
     03  B-HH                     PIC       Z9.
     03  FILLER                   PIC       X(01)  VALUE ":".
     03  B-MN                     PIC       Z9.
     03  FILLER                   PIC       X(01)  VALUE SPACE.
     03  FILLER                   PIC       N(12)  VALUE
         NC"］です。確認して下さい。".
*    線１
 01  SEN1               CHARACTER TYPE      IS     YA.
     03  FILLER                   PIC       N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                   PIC       N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                   PIC       N(18)  VALUE
         NC"──────────────────".
*    線２
 01  SEN2.
     03  FILLER                   PIC       X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                   PIC       X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                   PIC       X(36)  VALUE
         "------------------------------------".
*
     COPY   SNDTEDF  OF XFDLIB  JOINING   WK  AS   PREFIX.
*
*    数量編集
 01  WK-HEN                 PIC   9(09)V9(02).
 01  WK-HEN-R               REDEFINES   WK-HEN.
     03  WK-HEN-1           PIC   9(09).
     03  WK-HEN-2           PIC   9(02).
*    数量編集
 01  WK-HEN1.
     03  WK-HEN1-1          PIC   X(01).
     03  WK-HEN1-2          PIC   X(09).
     03  WK-HEN1-3          PIC   X(01).
     03  WK-HEN1-4          PIC   X(02).
*    日付編集
 01  WK-HEN-DATE.
     03  WK-HEN-DATE1       PIC   X(04).
     03  FILLER             PIC   X(01)  VALUE  "/".
     03  WK-HEN-DATE2       PIC   X(02).
     03  FILLER             PIC   X(01)  VALUE  "/".
     03  WK-HEN-DATE3       PIC   X(02).
*    時間編集
 01  WK-HEN-TIME.
     03  WK-HEN-TIME1       PIC   X(02).
     03  FILLER             PIC   X(01)  VALUE  ":".
     03  WK-HEN-TIME2       PIC   X(02).
*    日付変換１
 01  WK-HENKAN              PIC   9(08)  VALUE  ZERO.
 01  WK-HIDUKE.
     03  WK-HIDUKE1         PIC   9(04).
     03  WK-HIDUKE2         PIC   9(02).
     03  WK-HIDUKE3         PIC   9(02).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
 LINKAGE                SECTION.
 01  PARA-SOKO              PIC   X(02).
 01  PARA-TOKCD             PIC   9(08).
 01  PARA-SDEN              PIC   9(09).
 01  PARA-EDEN              PIC   9(09).
 01  PARA-KENSU             PIC   9(07).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-SOKO
                                       PARA-TOKCD
                                       PARA-SDEN
                                       PARA-EDEN
                                       PARA-KENSU.
*
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SNDTEDF.
     MOVE      "SNDTEDF "   TO   AB-FILE.
     MOVE      D01-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JYOKENF.
     MOVE      "JYOKENF "   TO   AB-FILE.
     MOVE      JYO-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENF.
     MOVE      "SHTDENF  "   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SNDSKKF.
     MOVE      "SNDSKKF"    TO   AB-FILE.
     MOVE      K01-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHOTBL1.
     MOVE      "SHOTBL1 "   TO   AB-FILE.
     MOVE      TBL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG    =   "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
*    システム日付８桁変換
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     MOVE     LINK-OUT-YMD8  TO   SYS-DATEW.
     OPEN     INPUT     JYOKENF SHTDENF SHOTBL1.
     OPEN     OUTPUT    SNDTEDF.
     OPEN     OUTPUT    SNDSKKF.
     OPEN     OUTPUT    PRINTF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG    RD-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
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
*
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE     ZERO           TO   DEN-F277.
     MOVE     ZERO           TO   DEN-F274.
     MOVE     PARA-SOKO      TO   DEN-F09.
     MOVE     PARA-SDEN      TO   DEN-F02.
     MOVE     ZERO           TO   DEN-F03.
     START    SHTDENF   KEY  >=   DEN-F277  DEN-F274
                                  DEN-F09   DEN-F02
                                  DEN-F04   DEN-F051
                                  DEN-F07   DEN-F112
                                  DEN-F03
         INVALID   KEY
              MOVE   "END"   TO   END-FLG
              DISPLAY NC"抽出対象データなし．" UPON CONS
              MOVE    4010     TO   PROGRAM-STATUS
              STOP    RUN
     END-START.
*    売上伝票Ｆ初期読み
     PERFORM  SHTDENF-READ-SEC.
     IF       END-FLG  =  "END"
              DISPLAY NC"抽出対象データなし．．" UPON CONS
              MOVE    4010     TO   PROGRAM-STATUS
              STOP    RUN
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    出力ファイル番号取得
     PERFORM  JYOKENF-READ-SEC.
*    売上伝票情報セット
     PERFORM URIAGE-SET-SEC
*    ファイル出力テーブル判定
     PERFORM TBLSET-SEC
*    売上伝票Ｆ読込み
     PERFORM  SHTDENF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*出力件数リスト出力
     PERFORM   LISTWT-SEC.
*
     MOVE      WK-RTCNT1 TO   PARA-KENSU.
     DISPLAY   NC"抽出件数＝" PARA-KENSU   UPON CONS.
*
*
     CLOSE     JYOKENF  SHTDENF  SHOTBL1.
     CLOSE     SNDTEDF.
     CLOSE     SNDSKKF.
     CLOSE     PRINTF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　倉庫出力先ファイル判定→ファイル出力            *
****************************************************************
 URIAGE-SET-SEC         SECTION.
*
     MOVE     "KENPIN-SET-SEC"  TO      S-NAME.
*
*送信用伝票データ出力
     MOVE     SPACE          TO   WK-REC.
     INITIALIZE                   WK-REC.
*取引先ＣＤ
     MOVE     DEN-F01        TO   WK-F01.
*伝票番号
     MOVE     DEN-F02        TO   WK-F02.
*行番号
     MOVE     DEN-F03        TO   WK-F03.
*店舗ＣＤ
     MOVE     DEN-F07        TO   WK-F04.
*注文日
     MOVE     DEN-F111       TO   WK-HENKAN.
     MOVE     WK-HENKAN      TO   WK-HIDUKE.
     MOVE     WK-HIDUKE1     TO   WK-HEN-DATE1.
     MOVE     WK-HIDUKE2     TO   WK-HEN-DATE2.
     MOVE     WK-HIDUKE3     TO   WK-HEN-DATE3.
     MOVE     WK-HEN-DATE    TO   WK-F05.
*納品日
     MOVE     DEN-F112       TO   WK-HENKAN.
     MOVE     WK-HENKAN      TO   WK-HIDUKE.
     MOVE     WK-HIDUKE1     TO   WK-HEN-DATE1.
     MOVE     WK-HIDUKE2     TO   WK-HEN-DATE2.
     MOVE     WK-HIDUKE3     TO   WK-HEN-DATE3.
     MOVE     WK-HEN-DATE    TO   WK-F06.
*分類ＣＤ
     MOVE     DEN-F12        TO   WK-F07.
*自社商品ＣＤ
     MOVE     DEN-F141       TO   WK-F08.
*商品名称１
     MOVE     DEN-F1421      TO   WK-F09.
*商品名称２
     MOVE     DEN-F1422      TO   WK-F10.
*数量
     INITIALIZE                   WK-HEN1.
     IF       DEN-F15  <  ZERO
              MOVE   "-"     TO   WK-HEN1-1
     END-IF.
     MOVE     DEN-F15        TO   WK-HEN.
     MOVE     WK-HEN-1       TO   WK-HEN1-2.
     MOVE     "."            TO   WK-HEN1-3.
     MOVE     WK-HEN-2       TO   WK-HEN1-4.
     MOVE     WK-HEN1        TO   WK-F11.
*原価単価
     INITIALIZE                   WK-HEN1.
     IF       DEN-F172  <  ZERO
              MOVE   "-"     TO   WK-HEN1-1
     END-IF.
     MOVE     DEN-F172       TO   WK-HEN.
     MOVE     WK-HEN-1       TO   WK-HEN1-2.
     MOVE     "."            TO   WK-HEN1-3.
     MOVE     WK-HEN-2       TO   WK-HEN1-4.
     MOVE     WK-HEN1        TO   WK-F12.
*売価単価
     INITIALIZE                   WK-HEN1.
     IF       DEN-F173  <  ZERO
              MOVE   "-"     TO   WK-HEN1-1
     END-IF.
     MOVE     DEN-F173       TO   WK-HEN.
     MOVE     WK-HEN-1       TO   WK-HEN1-2.
     MOVE     "."            TO   WK-HEN1-3.
     MOVE     WK-HEN-2       TO   WK-HEN1-4.
     MOVE     WK-HEN1        TO   WK-F13.
*指定商品ＣＤ（取引先商品ＣＤ）
     MOVE     DEN-F25        TO   WK-F14.
*オンライン区分
     MOVE     DEN-F274       TO   WK-F15.
*店舗名称（カナ）
     MOVE     DEN-F30        TO   WK-F16.
*受信日
     MOVE     SYS-DATEW      TO   WK-HENKAN.
     MOVE     WK-HENKAN      TO   WK-HIDUKE.
     MOVE     WK-HIDUKE1     TO   WK-HEN-DATE1.
     MOVE     WK-HIDUKE2     TO   WK-HEN-DATE2.
     MOVE     WK-HIDUKE3     TO   WK-HEN-DATE3.
     MOVE     WK-HEN-DATE    TO   WK-F17.
*受信時間
     MOVE     SYS-HH         TO   WK-HEN-TIME1.
     MOVE     SYS-MN         TO   WK-HEN-TIME2.
     MOVE     WK-HEN-TIME    TO   WK-F18.
*振分倉庫ＣＤ
     MOVE     DEN-F09        TO   WK-F19.
*_番
     MOVE     DEN-F49        TO   WK-F20.
*訂正前数量
     INITIALIZE                   WK-HEN1.
     IF       DEN-F50  <  ZERO
              MOVE   "-"     TO   WK-HEN1-1
     END-IF.
     MOVE     DEN-F50        TO   WK-HEN.
     MOVE     WK-HEN-1       TO   WK-HEN1-2.
     MOVE     "."            TO   WK-HEN1-3.
     MOVE     WK-HEN-2       TO   WK-HEN1-4.
     MOVE     WK-HEN1        TO   WK-F21.
*取引先ＣＤセット
*    IF       DEN-F132  =  "99"
*             MOVE  "000760" TO   WK-F22(40:6)
*    ELSE
*             MOVE  "000173" TO   WK-F22(40:6)
*    END-IF.
*改行コード
     MOVE     X"0D0A"        TO   WK-F22.
*  商品変換テーブル検索
     MOVE     DEN-F01        TO   TBL-F01.
     MOVE     DEN-F25        TO   TBL-F02.
     READ     SHOTBL1
       INVALID
              CONTINUE
       NOT  INVALID
              MOVE TBL-F08   TO   WK-F20
     END-READ.
*
 URIAGE-SET-EXIT.
     EXIT.
****************************************************************
*　　　　　　　倉庫出力先ファイル判定→ファイル出力            *
****************************************************************
 TBLSET-SEC            SECTION.
*
     MOVE     "TBLSET-SEC"   TO      S-NAME.
*送信用伝票データ出力
     MOVE     SPACE          TO   D01-REC.
     INITIALIZE                   D01-REC.
     MOVE     WK-REC         TO   D01-REC.
     WRITE    D01-REC.
     ADD      1              TO   WK-RTCNT1.
*
 TBLSET-EXIT.
     EXIT.
****************************************************************
*　　　　　　　条件ファイル読込み　　　　　　　　　　　　　　*
****************************************************************
 JYOKENF-READ-SEC      SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE      "20"      TO      JYO-F01.
     MOVE      DEN-F09   TO      JYO-F02.
     READ      JYOKENF   INVALID
               DISPLAY "JYOKENF INVALID KEY = "
                        JYO-F01 ":" JYO-F02  UPON CONS
               STOP  RUN
       NOT INVALID
*             EVALUATE  JYO-F04
                            MOVE  JYO-F04  TO  WK-FLCD1
                            MOVE  DEN-F48  TO  WK-RTCD1
                            MOVE  JYO-F03  TO  WK-RTNM1
*             END-EVALUATE
     END-READ.
*
 JYOKENF-READ-EXIT.
     EXIT.
****************************************************************
*           リスト出力処理                          3.1.1      *
****************************************************************
 LISTWT-SEC   SECTION.
*
     MOVE      SYS-YY         TO        YY.
     MOVE      SYS-MM         TO        MM.
     MOVE      SYS-DD         TO        DD.
     MOVE      1              TO        PEIJI.
     WRITE     PRINT-REC      FROM      MIDASHI1 AFTER 2.
     WRITE     PRINT-REC      FROM      SEN1     AFTER 2.
     WRITE     PRINT-REC      FROM      MIDASHI2 AFTER 1.
     WRITE     PRINT-REC      FROM      SEN1     AFTER 1.
 LISTWT-010.
               MOVE      WK-FLCDT       TO       FILECD
               MOVE      WK-RTNMT       TO       ROUTENM
               MOVE      WK-RTCDT       TO       ROUTECD
               MOVE      WK-RTCNTT      TO       DATASU
         IF    WK-FLCDT       NOT =     ZERO
               MOVE  SPACE         TO   K01-REC
               INITIALIZE               K01-REC
               MOVE  WK-RTCNTT     TO   K01-F01
               MOVE  X"0D0A"       TO   K01-F02
               WRITE K01-REC
               WRITE     PRINT-REC FROM MEISAI   AFTER 1
               WRITE     PRINT-REC FROM SEN2     AFTER 1
         END-IF
     MOVE      SYS-YY-W         TO      B-YY.
     MOVE      SYS-MM-W         TO      B-MM.
     MOVE      SYS-DD-W         TO      B-DD.
     MOVE      SYS-HH           TO      B-HH.
     MOVE      SYS-MN           TO      B-MN.
     WRITE     PRINT-REC      FROM      KOMENTO  AFTER 2.
 LISTWT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　売上伝票ファイル　ＲＥＡＤ　　　　　　　　　　　*
****************************************************************
 SHTDENF-READ-SEC      SECTION.
*
     MOVE "SHTDENF-READ-SEC"   TO   S-NAME.
*
     READ     SHTDENF
              NEXT AT END
                 MOVE   "END"  TO   END-FLG
                 GO            TO   SHTDENF-READ-EXIT
              NOT AT END
                 ADD     1     TO   RD-CNT
     END-READ.
*終了条件
*パラ倉庫ＣＤ
     IF  DEN-F09        NOT  =    PARA-SOKO
         MOVE     "END"      TO   END-FLG
         MOVE      ZERO      TO   SKIP-FLG
         GO             TO   SHTDENF-READ-EXIT
     END-IF.
*パラ取引先ＣＤ
     IF  DEN-F01        NOT  =    PARA-TOKCD
         MOVE      ZERO      TO   SKIP-FLG
         GO             TO   SHTDENF-READ-SEC
     END-IF.
*伝票番号範囲チェック
     IF  DEN-F02             <    PARA-SDEN
     OR  DEN-F02             >    PARA-EDEN
         MOVE      ZERO      TO   SKIP-FLG
         GO             TO   SHTDENF-READ-SEC
     END-IF.
*売上作成ＦＬＧ
     IF  DEN-F277            =    9
         MOVE      ZERO      TO   SKIP-FLG
         GO             TO   SHTDENF-READ-SEC
     END-IF.
*オンライン
     IF  DEN-F274       NOT  =    0
         MOVE      "END"     TO   END-FLG
         MOVE      ZERO      TO   SKIP-FLG
         GO             TO   SHTDENF-READ-EXIT
     END-IF.
*読み飛ばし条件
*行_（８０の場合）
     IF  DEN-F03        =    80
         GO             TO   SHTDENF-READ-SEC
     END-IF.
*行_（９０の場合）
     IF  DEN-F03        =    90
         MOVE      ZERO      TO   SKIP-FLG
         GO             TO   SHTDENF-READ-SEC
     END-IF.
*相殺区分
     IF  DEN-F04        NOT  =    0
         MOVE      ZERO      TO   SKIP-FLG
         GO             TO   SHTDENF-READ-SEC
     END-IF.
*伝区
     IF  DEN-F051       NOT  =    40
         MOVE      ZERO      TO   SKIP-FLG
         GO             TO   SHTDENF-READ-SEC
     END-IF.
*伝発
     IF  DEN-F134       NOT  =    0
         MOVE      ZERO      TO   SKIP-FLG
         GO             TO   SHTDENF-READ-SEC
     END-IF.
*倉庫ＣＤが未設定の場合、読み飛ばし
     IF  DEN-F09        =    SPACE
         MOVE      ZERO      TO   SKIP-FLG
         GO             TO   SHTDENF-READ-SEC
     END-IF.
*振分対象伝票番号退避／売上フラグチェック
     MOVE    DEN-F02    TO   WK-DENPYO.
     MOVE    1          TO   SKIP-FLG.
*
 SHTDENF-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```

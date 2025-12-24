****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷検品システム　　　　　　　　　*
*    業務名　　　　　　　：　オンラインデータ抽出　　　　　　　*
*    モジュール名　　　　：　オンラインデータ抽出処理　　　　　*
*    作成日／更新日　　　：　2000/10/10                        *
*    作成者／更新者　　　：　T.TAKAHASHI                       *
*    処理概要　　　　　　：　受け取ったパラメタのバッチNO.     *
*                            倉庫コードより該当のデータを      *
*                            抽出する。                        *
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*　                                                            *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SKE9992R.
 AUTHOR.                NAV.
 DATE-WRITTEN.          00/10/10.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         YA        IS   YA
         YB-21     IS   YB-21
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*売上伝票データ
     SELECT   SHTDENLA  ASSIGN    TO        DA-01-VI-SHTDENLA
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F46   DEN-F47
                                            DEN-F01   DEN-F48
                                            DEN-F02   DEN-F04
                                            DEN-F051  DEN-F03
                        FILE      STATUS    IS   DEN-STATUS.
*----<< 累積検品明細Ｆ >>--*
     SELECT   RUISYUF   ASSIGN         DA-01-VI-RUISYUL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  RUI-F04 RUI-F06 RUI-F07
                        STATUS         RUI-STATUS.
*送信用伝票データ
     SELECT   SNDDENF   ASSIGN    TO        SNDDENF
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    IS   SDE-STATUS.
*商品変換テーブル
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE      STATUS    IS   TBL-STATUS.
*送信用件数Ｆ
     SELECT   SNDDEKF   ASSIGN         DA-01-S-SNDDEKF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         DEK-STATUS.
*プリントＦ
     SELECT      PRINTF      ASSIGN    TO        LP-04-PRTF.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENLA
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
*----<< 累積検品明細Ｆ >>--*
 FD  RUISYUF            LABEL RECORD   IS   STANDARD
     BLOCK              CONTAINS       1    RECORDS.
     COPY        RUISYUF     OF      XFDLIB
                 JOINING     RUI     PREFIX.
******************************************************************
*    送信用売上伝票データ
******************************************************************
 FD  SNDDENF            LABEL RECORD   IS   STANDARD.
     COPY     SNDDENF   OF        XFDLIB
              JOINING   SDE       PREFIX.
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  SHOTBL1            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
******************************************************************
*    送信件数ファイル
******************************************************************
 FD  SNDDEKF            BLOCK CONTAINS 1    RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  DEK-REC.
     03  DEK-F01             PIC  9(08).
     03  DEK-F02             PIC  X(02).
******************************************************************
*    プリントファイル
******************************************************************
 FD  PRINTF.
 01  PRINT-REC                    PIC       X(200).
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WT-CNT                  PIC  9(08)     VALUE  ZERO.
 01  RUISYUF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  WK-SDE-F11              PIC  9(13)     VALUE  ZERO.
 01  WK-SDE-F21              PIC  9(13)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE.
         05  SYS-YY        PIC 9(02).
         05  SYS-MM        PIC 9(02).
         05  SYS-DD        PIC 9(02).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  SDE-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  DEK-STATUS        PIC  X(02).
     03  RUI-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SKE9992R".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SKE9992R".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SKE9992R".
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
*    見出し行１
 01  MIDASHI1.
     03  FILLER                   PIC       X(37)  VALUE SPACE.
     03  FILLER                   PIC       N(18)  VALUE
       NC"【検品送信前チェック処理確認リスト】"
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
     03  FILLER                   PIC       X(50)  VALUE SPACE.
     03  MIDASHI2-01              PIC       N(17).
     03  KENSU                    PIC       ZZ,ZZZ,ZZ9.
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
 LINKAGE                SECTION.
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-NOUDT             PIC   9(08).
 01  PARA-CHK               PIC   9(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-SOKO
                                       PARA-NOUDT
                                       PARA-CHK.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENLA.
     MOVE      "SHTDENLA"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SNDDENF.
     MOVE      "SNDDENF "   TO   AB-FILE.
     MOVE      SDE-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHOTBL1.
     MOVE      "SHOTBL1 "   TO   AB-FILE.
     MOVE      TBL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   RUISYUF.
     MOVE      "RUISYUL1"   TO   AB-FILE.
     MOVE      RUI-STATUS   TO   AB-STS.
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
     PERFORM  MAIN-SEC
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     SHTDENLA  SHOTBL1   RUISYUF.
     OPEN     OUTPUT    SNDDENF   SNDDEKF   PRINTF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WT-CNT.
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
     MOVE     PARA-JDATE     TO   DEN-F46.
     MOVE     PARA-JTIME     TO   DEN-F47.
     MOVE     PARA-TORICD    TO   DEN-F01.
     MOVE     SPACE          TO   DEN-F48.
     START    SHTDENLA  KEY  >=   DEN-F46   DEN-F47
                                  DEN-F01   DEN-F48
                                  DEN-F02   DEN-F04
                                  DEN-F051  DEN-F03
         INVALID   KEY
              MOVE      9    TO   END-FG
              GO   TO   INIT-EXIT
     END-START.
*
 INIT-010.
*
     PERFORM  SHTDENF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    チェックＦＬＧが１の時はデータ作成は行なわない。
     IF       PARA-CHK  =  1
              GO             TO   MAIN-000
     END-IF.
*
*送信用伝票データ出力
     MOVE     SPACE          TO   SDE-REC.
     INITIALIZE                   SDE-REC.
*取引先ＣＤ
     MOVE     DEN-F01        TO   SDE-F01.
*伝票番号
     MOVE     DEN-F02        TO   SDE-F02.
*行番号
     MOVE     DEN-F03        TO   SDE-F03.
*店舗ＣＤ
     MOVE     DEN-F07        TO   SDE-F04.
*注文日
     MOVE     DEN-F111       TO   WK-HENKAN.
     MOVE     WK-HENKAN      TO   WK-HIDUKE.
     MOVE     WK-HIDUKE1     TO   WK-HEN-DATE1.
     MOVE     WK-HIDUKE2     TO   WK-HEN-DATE2.
     MOVE     WK-HIDUKE3     TO   WK-HEN-DATE3.
     MOVE     WK-HEN-DATE    TO   SDE-F05.
*納品日
     MOVE     DEN-F112       TO   WK-HENKAN.
     MOVE     WK-HENKAN      TO   WK-HIDUKE.
     MOVE     WK-HIDUKE1     TO   WK-HEN-DATE1.
     MOVE     WK-HIDUKE2     TO   WK-HEN-DATE2.
     MOVE     WK-HIDUKE3     TO   WK-HEN-DATE3.
     MOVE     WK-HEN-DATE    TO   SDE-F06.
*分類ＣＤ
     MOVE     DEN-F12        TO   SDE-F07.
*自社商品ＣＤ
     MOVE     DEN-F141       TO   SDE-F08.
*商品名称１
     MOVE     DEN-F1421      TO   SDE-F09.
*商品名称２
     MOVE     DEN-F1422      TO   SDE-F10.
*数量
     INITIALIZE                   WK-HEN1.
     IF       DEN-F15  <  ZERO
              MOVE   "-"     TO   WK-HEN1-1
     END-IF.
*    MOVE     DEN-F15        TO   WK-HEN.
*    MOVE     WK-HEN-1       TO   WK-HEN1-2.
*    MOVE     "."            TO   WK-HEN1-3.
*    MOVE     WK-HEN-2       TO   WK-HEN1-4.
*    MOVE     WK-HEN1        TO   SDE-F11.
     COMPUTE  WK-SDE-F11  =  DEN-F15  *  100.
     MOVE     WK-SDE-F11     TO   SDE-F11.
*原価単価
     INITIALIZE                   WK-HEN1.
     IF       DEN-F172  <  ZERO
              MOVE   "-"     TO   WK-HEN1-1
     END-IF.
     MOVE     DEN-F172       TO   WK-HEN.
     MOVE     WK-HEN-1       TO   WK-HEN1-2.
     MOVE     "."            TO   WK-HEN1-3.
     MOVE     WK-HEN-2       TO   WK-HEN1-4.
     MOVE     WK-HEN1        TO   SDE-F12.
*売価単価
     INITIALIZE                   WK-HEN1.
     IF       DEN-F173  <  ZERO
              MOVE   "-"     TO   WK-HEN1-1
     END-IF.
     MOVE     DEN-F173       TO   WK-HEN.
     MOVE     WK-HEN-1       TO   WK-HEN1-2.
     MOVE     "."            TO   WK-HEN1-3.
     MOVE     WK-HEN-2       TO   WK-HEN1-4.
     MOVE     WK-HEN1        TO   SDE-F13.
*指定商品ＣＤ（取引先商品ＣＤ）
     MOVE     DEN-F25        TO   SDE-F14.
*オンライン区分
     MOVE     DEN-F274       TO   SDE-F15.
*店舗名称（カナ）
     MOVE     DEN-F30        TO   SDE-F16.
*受信日
     MOVE     DEN-F46        TO   WK-HENKAN.
     MOVE     WK-HENKAN      TO   WK-HIDUKE.
     MOVE     WK-HIDUKE1     TO   WK-HEN-DATE1.
     MOVE     WK-HIDUKE2     TO   WK-HEN-DATE2.
     MOVE     WK-HIDUKE3     TO   WK-HEN-DATE3.
     MOVE     WK-HEN-DATE    TO   SDE-F17.
*受信時間
     MOVE     DEN-F471       TO   WK-HEN-TIME1.
     MOVE     DEN-F472       TO   WK-HEN-TIME2.
     MOVE     WK-HEN-TIME    TO   SDE-F18.
*振分倉庫ＣＤ
     MOVE     DEN-F48        TO   SDE-F19.
*_番
     MOVE     DEN-F49        TO   SDE-F20.
*訂正前数量
     INITIALIZE                   WK-HEN1.
     IF       DEN-F50  <  ZERO
              MOVE   "-"     TO   WK-HEN1-1
     END-IF.
*    MOVE     DEN-F50        TO   WK-HEN.
*    MOVE     WK-HEN-1       TO   WK-HEN1-2.
*    MOVE     "."            TO   WK-HEN1-3.
*    MOVE     WK-HEN-2       TO   WK-HEN1-4.
*    MOVE     WK-HEN1        TO   SDE-F21.
     COMPUTE  WK-SDE-F21  =  DEN-F50  *  100.
     MOVE     WK-SDE-F21     TO   SDE-F21.
*取引先固有エリア
     MOVE     DEN-XI0004     TO   SDE-F22.
*  商品変換テーブル検索
     MOVE     DEN-F01        TO   TBL-F01.
     MOVE     DEN-F25        TO   TBL-F02.
     READ     SHOTBL1
       INVALID
              CONTINUE
       NOT  INVALID
              MOVE TBL-F08   TO   SDE-F20
**************サカタＣＤ再セット
              MOVE TBL-F03   TO   SDE-F08
     END-READ.
*改行コード
     MOVE     X"0D0A"        TO   SDE-F23.
*
     WRITE    SDE-REC.
*
 MAIN-000.
     ADD      1              TO   WT-CNT.
*
 MAIN-010.
*
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
*    件数リスト発行
     IF  PARA-CHK  = 1
         MOVE  NC"指定バック番号未検品ＤＴ　＝　" TO MIDASHI2-01
     ELSE
         MOVE  NC"未検品ＤＴ作成件数　＝　　　　" TO MIDASHI2-01
     END-IF.
*
     PERFORM LISTWT-SEC.
*件数Ｆ出力
     MOVE     SPACE               TO   DEK-REC.
     INITIALIZE                        DEK-REC.
*
     MOVE      WT-CNT             TO   DEK-F01.
     MOVE      X"0D0A"            TO   DEK-F02.
*
     WRITE     DEK-REC.
*
     CLOSE     SHTDENLA  SNDDENF  SHOTBL1  PRINTF  SNDDEKF
               RUISYUF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 SHTDENF-READ-SEC      SECTION.
*
     MOVE "SHTDENF-READ-SEC"   TO   S-NAME.
*
     READ     SHTDENLA
              AT END
                 MOVE    9     TO   END-FG
                 GO            TO   SHTDENF-READ-EXIT
              NOT AT END
                 ADD     1     TO   RD-CNT
     END-READ.
*    検品完了ＦＬＧチェック（検品済は読み飛ばし）
     IF       DEN-F279  =  1
              GO               TO   SHTDENF-READ-SEC
     END-IF.
     IF     ( PARA-JDATE     =    DEN-F46 ) AND
            ( PARA-JTIME     =    DEN-F47 ) AND
            ( PARA-TORICD    =    DEN-F01 )
*********IF   PARA-SOKO      NOT =    SPACE
*             IF   PARA-SOKO      =   DEN-F48
*                  CONTINUE
*             ELSE
*                  IF   PARA-SOKO      <    DEN-F48
*                       MOVE      9    TO   END-FG
*                       GO   TO   SHTDENF-READ-EXIT
*                  ELSE
*                       GO   TO   SHTDENF-READ-SEC
*                  END-IF
*             END-IF
*********END-IF
              CONTINUE
     ELSE
         MOVE      9         TO   END-FG
         GO        TO        SHTDENF-READ-EXIT
     END-IF.
*    納品日チェック
     IF       DEN-F112   <=     PARA-NOUDT
              CONTINUE
     ELSE
              GO               TO   SHTDENF-READ-SEC
     END-IF.
*    検品累積Ｆ確認
     PERFORM  RUISYUF-READ-SEC.
*    存在チェック
     IF       RUISYUF-INV-FLG = "INV"
              CONTINUE
     ELSE
              GO               TO   SHTDENF-READ-SEC
     END-IF.
*
 SHTDENF-READ-EXIT.
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
     MOVE      WT-CNT         TO        KENSU.
     WRITE     PRINT-REC      FROM      MIDASHI1 AFTER 2.
     WRITE     PRINT-REC      FROM      SEN1     AFTER 2.
     WRITE     PRINT-REC      FROM      MIDASHI2 AFTER 1.
     WRITE     PRINT-REC      FROM      SEN1     AFTER 1.
*
 LISTWT-EXIT.
     EXIT.
****************************************************************
*           リスト出力処理                          3.1.1      *
****************************************************************
 RUISYUF-READ-SEC      SECTION.
*
     MOVE      DEN-XI0004(42:4) TO      RUI-F04.
     MOVE      DEN-F02          TO      RUI-F06.
     MOVE      DEN-F03          TO      RUI-F07.
     READ      RUISYUF
               INVALID     MOVE "INV"   TO  RUISYUF-INV-FLG
               NOT INVALID MOVE SPACE   TO  RUISYUF-INV-FLG
     END-READ.
*
 RUISYUF-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＤＣＭＪＡＰＡＮ　ＷＥＢＥＤＩ　　*
*    業務名　　　　　　　：　ＤＣＭＪＡＰＡＮ　ＷＥＢＥＤＩ　　*
*    モジュール名　　　　：　納品予定データ送信形式変換        *
*    作成日／更新日　　　：　2007/05/17                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受け取ったパラメタより対象データ  *
*                            を出荷情報保存データより抽出する。*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY8705B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          07/05/17.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*ＤＣＭＪＡＰＡＮ納品予定データ
     SELECT   DJSYUKF   ASSIGN    TO        DA-01-VI-DJSYUKL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DJS-K01   DJS-K02
                                            DJS-K03   DJS-K04
                                            DJS-K08
                        FILE      STATUS    DJS-STATUS.
*ＤＣＭＪＡＰＡＮ情報データ
     SELECT   DJJOHOF   ASSIGN    TO        DA-01-VI-DJJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DJJ-K01   DJJ-K02
                                            DJJ-K03   DJJ-K04
                                            DJJ-K05   DJJ-K06
                                            DJJ-K07   DJJ-K08
                        FILE      STATUS    DJJ-STATUS.
*ＤＣＭＪＡＰＡＮ送信ファイル
     SELECT   SNDDCMJP  ASSIGN    TO        DA-01-S-SNDDCMJP
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SND-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    ＤＣＭＪＡＰＡＮ納品予定ファイル
******************************************************************
 FD  DJSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     DJSYUKF   OF        XFDLIB
              JOINING   DJS       PREFIX.
******************************************************************
*    ＤＣＭＪＡＰＡＮ情報データ
******************************************************************
 FD  DJJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     DJJOHOF   OF        XFDLIB
              JOINING   DJJ       PREFIX.
******************************************************************
*    ＤＣＭＪＡＰＡＮ送信ファイル
******************************************************************
 FD  SNDDCMJP
                        BLOCK CONTAINS 1 RECORDS.
 01  SND-REC            PIC  X(3149).
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  DJJOHOF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP-CNT            PIC  9(08)     VALUE  ZERO.
     03  SND-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-DENNO                PIC  9(09)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  SND-STATUS        PIC  X(02).
     03  DJS-STATUS        PIC  X(02).
     03  DJJ-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY8705B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY8705B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY8705B".
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
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-NOUDT             PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-SOKO
                                       PARA-NOUDT.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DJSYUKF.
     MOVE      "DJSYUKL2"    TO   AB-FILE.
     MOVE      DJS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SNDDCMJP.
     MOVE      "SNDDCMJP"    TO   AB-FILE.
     MOVE      SND-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DJJOHOF.
     MOVE      "DJJOHOL1"    TO   AB-FILE.
     MOVE      DJJ-STATUS   TO   AB-STS.
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
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       DJSYUKF   DJJOHOF.
     OPEN     EXTEND    SNDDCMJP.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
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
*    ＤＣＭＪＡＰＡＮ出荷情報スタート
     MOVE     SPACE          TO   DJS-REC.
     INITIALIZE                   DJS-REC.
     MOVE     PARA-JDATE     TO   DJS-K01.
     MOVE     PARA-JTIME     TO   DJS-K02.
     MOVE     PARA-TORICD    TO   DJS-K03.
     MOVE     PARA-SOKO      TO   DJS-K04.
     MOVE     PARA-NOUDT     TO   DJS-K08.
     START    DJSYUKF   KEY  >=   DJS-K01   DJS-K02
                                  DJS-K03   DJS-K04
                                  DJS-K08
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    ＤＣＭＪＡＰＡＮ出荷情報保存データ読込み
     PERFORM DJSYUKF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 DJSYUKF-READ-SEC    SECTION.
*
     READ     DJSYUKF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  DJSYUKF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
     IF    READ-CNT(6:3)  =  "000" OR "500"
           DISPLAY "READ-CNT = " READ-CNT  UPON CONS
     END-IF.
*    送信ＦＬＧのチェック
     IF       DJS-K11  =  "1"
              GO                 TO   DJSYUKF-READ-SEC
     END-IF.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  DJS-K01
     AND      PARA-JTIME  =  DJS-K02
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  DJSYUKF-READ-EXIT
     END-IF.
*倉庫ＣＤチェック
     IF       PARA-SOKO   =  SPACE
              CONTINUE
     ELSE
*             抽出条件のチェック
              IF       PARA-SOKO   =  DJS-K04
                       CONTINUE
              ELSE
                       GO        TO   DJSYUKF-READ-SEC
              END-IF
     END-IF.
*納品日のチェック
     IF       PARA-NOUDT  =  ZERO
              CONTINUE
     ELSE
*             抽出条件のチェック
              IF       PARA-NOUDT  =  DJS-K08
                       CONTINUE
              ELSE
                       GO        TO   DJSYUKF-READ-SEC
              END-IF
     END-IF.
*
 DJSYUKF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    送信ファイル出力
     MOVE     SPACE               TO   SND-REC.
     INITIALIZE                        SND-REC.
*ヘッダ部出力
     MOVE     "0"                 TO   DJS-A28(48:1).
     MOVE     DJS-K20             TO   SND-REC.
     IF       SND-REC(23:1)   =   ZERO
    AND       SND-REC(22:1)   =   SPACE
              MOVE     SPACE      TO   SND-REC(23:1)
     END-IF.
*    ヘッダを書き込むかどうか判断します。
     IF       DJS-K06    =   WK-DENNO
              CONTINUE
     ELSE
              WRITE    SND-REC
              ADD      1          TO   SND-CNT
              MOVE     DJS-K06    TO   WK-DENNO
     END-IF.
*明細部出力
*    送信ファイル出力
     MOVE     SPACE               TO   SND-REC.
     INITIALIZE                        SND-REC.
*    ダミーデータセット
*****MOVE     "0"                 TO   DJS-A43(6:1).
     MOVE     "000000"            TO   DJS-A43.
     MOVE     DJS-K21             TO   SND-REC.
     IF       DJS-F304  =   "000880"
     OR       DJS-F304  =   "000882"
     OR       DJS-F304  =   "000883"
     OR       DJS-F304  =   "001427"
     OR       DJS-F304  =   "014272"
     OR       DJS-F304  =   "014273"
              MOVE      SPACE     TO   SND-REC(135:2)
     END-IF.
     WRITE    SND-REC.
     ADD      1                   TO   SND-CNT.
*情報データにも書込み
     MOVE     SPACE               TO   DJJ-REC.
     INITIALIZE                        DJJ-REC.
     MOVE     DJS-K01             TO   DJJ-K01.
     MOVE     DJS-K02             TO   DJJ-K02.
     MOVE     DJS-K03             TO   DJJ-K03.
     MOVE     DJS-K04             TO   DJJ-K04.
     MOVE     DJS-K05             TO   DJJ-K05.
     MOVE     DJS-K06             TO   DJJ-K06.
     MOVE     DJS-K07             TO   DJJ-K07.
     MOVE     DJS-K08             TO   DJJ-K08.
     READ     DJJOHOF  INVALID
              MOVE     "INV"      TO   DJJOHOF-INV-FLG
              NOT  INVALID
              MOVE     SPACE      TO   DJJOHOF-INV-FLG
     END-READ.
*
     IF       DJJOHOF-INV-FLG = SPACE
              MOVE     SYS-DATEW           TO   DJJ-K12
              MOVE     "1"                 TO   DJJ-K11
              REWRITE  DJJ-REC
     END-IF.
*送信情報セット
     MOVE     SYS-DATEW           TO   DJS-K12.
     MOVE     "1"                 TO   DJS-K11.
     REWRITE  DJS-REC.
*
 MAIN010.
*    ＤＣＭＪＡＰＡＮ出荷情報保存データ読込み
     PERFORM DJSYUKF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "DCMJAPANｼｭｯｶ   READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "DCMJAPANｼｭｯｶ ｿｳｼﾝ   CNT = " SND-CNT    UPON CONS.
*
     CLOSE    DJSYUKF  DJJOHOF  SNDDCMJP.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

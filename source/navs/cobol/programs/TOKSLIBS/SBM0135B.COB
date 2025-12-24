****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　イオン流通ＢＭＳ　　　　　　　　　*
*    業務名　　　　　　　：　データ抽出処理　　　　　　　　　　*
*    モジュール名　　　　：　受領訂正データ抽出                *
*    作成日／更新日　　　：　2012/11/14                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　流通ＢＭＳ受領訂正メッセージより　*
*                            受領訂正ワークを出力する。　　　　*
*    作成日／更新日　　　：　2019/09/20                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　消費税軽減税率対応　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SBM0135B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          12/11/14.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受領訂正メッセージ受信日
     SELECT   BMSJYTF1  ASSIGN    TO        DA-01-VI-BMSJYTL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JYT1-F013 JYT1-F011
                        FILE  STATUS   IS   JYT1-STS.
*受領訂正メッセージ計上日
     SELECT   BMSJYTF2  ASSIGN    TO        DA-01-VI-BMSJYTL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JYT2-F013 JYT2-F329
                        FILE  STATUS   IS   JYT2-STS.
*受領訂正ワーク
     SELECT   BMSJYTW   ASSIGN    TO        DA-01-VS-BMSJYTW
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE  STATUS   IS   JTW-STS.
*受領メッセージ
     SELECT   BMSJYRF   ASSIGN    TO        DA-01-VI-BMSJYRL4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYR-F013 JYR-F302
                                            JYR-F402
                        FILE  STATUS   IS   JYR-STS.
*取引先マスタ
     SELECT  HTOKMS     ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE  STATUS   IS   TOK-STS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受領訂正メッセージ受信日　　　　
******************************************************************
 FD  BMSJYTF1           LABEL RECORD   IS   STANDARD.
     COPY     BMSJYTF   OF        XFDLIB
              JOINING   JYT1      PREFIX.
******************************************************************
*    受領訂正メッセージ計上日　　　　
******************************************************************
 FD  BMSJYTF2           LABEL RECORD   IS   STANDARD.
     COPY     BMSJYTF   OF        XFDLIB
              JOINING   JYT2      PREFIX.
******************************************************************
*    受領訂正ワーク
******************************************************************
 FD  BMSJYTW            LABEL RECORD   IS   STANDARD.
     COPY     BMSJYTW   OF        XFDLIB
              JOINING   JTW       PREFIX.
******************************************************************
*    受領メッセージ
******************************************************************
 FD  BMSJYRF            LABEL RECORD   IS   STANDARD.
     COPY     BMSJYRF   OF        XFDLIB
              JOINING   JYR       PREFIX.
******************************************************************
*    取引先マスタ
******************************************************************
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  READ-CNT                PIC  9(07)     VALUE  ZERO.
 01  WRITE-CNT               PIC  9(07)     VALUE  ZERO.
 01  UNMACH-CNT              PIC  9(07)     VALUE  ZERO.
*
 01  JYR-INVSW               PIC  X(03).
*
*
******************************************************************
*    受領訂正メッセージ　ワーク定期　
******************************************************************
** WJYT-REC.
     COPY     BMSJYTF   OF        XFDLIB
              JOINING   WJYT      PREFIX.
*システム日付の編集
 01  SYS-WORKAREA.
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*
 01  STS-AREA.
     03  JYT1-STS          PIC  X(02).
     03  JYT2-STS          PIC  X(02).
     03  JTW-STS           PIC  X(02).
     03  JYR-STS           PIC  X(02).
     03  TOK-STS           PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SBM0135B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SBM0135B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SBM0135B".
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
*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
*01  PARA-AREA.
 01  PARA-TOKCD     PIC   9(08).
 01  PARA-HSYU      PIC   X(01).
 01  PARA-HFROM     PIC   9(08).
 01  PARA-HTO       PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING   PARA-TOKCD  PARA-HSYU
                                         PARA-HFROM  PARA-HTO.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   BMSJYTF1.
     MOVE      "BMSJYTL2"   TO   AB-FILE.
     MOVE      JYT1-STS     TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   BMSJYTF2.
     MOVE      "BMSJYTL3"  TO   AB-FILE.
     MOVE      JYT2-STS     TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   BMSJYTW.
     MOVE      "BMSJYTW"   TO   AB-FILE.
     MOVE      JTW-STS      TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   BMSJYRF.
     MOVE      "BMSJYRL4"  TO   AB-FILE.
     MOVE      JYR-STS      TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTOKMS.
     MOVE      "TOKMS2"    TO   AB-FILE.
     MOVE      TOK-STS      TO   AB-STS.
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
*
     IF  PARA-HSYU  =  "1"
         OPEN  INPUT    BMSJYTF1
     END-IF.
     IF  PARA-HSYU  =  "2"
         OPEN  INPUT    BMSJYTF2
     END-IF.
*
     MOVE     SPACE               TO   WJYT-REC.
     INITIALIZE                        WJYT-REC.
     OPEN     INPUT     BMSJYRF   HTOKMS.
     OPEN     OUTPUT    BMSJYTW.
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
*ファイルスタート
     PERFORM  INSTART-SEC.
     IF   END-FLG = "END"
      DISPLAY NC"＃＃　受領訂正メッセージ無　＃＃"  UPON CONS
          GO                    TO   INIT-EXIT
     END-IF.
*ファイル読込
     PERFORM INREAD-SEC.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    受領訂正メッセージスタート
****************************************************************
 INSTART-SEC                SECTION.
*
     MOVE    "INSTART-SEC"        TO   S-NAME.
*
     IF  PARA-HSYU  =  "2"
         GO  TO   INSTART-200
     END-IF.
*
     MOVE     PARA-TOKCD          TO   JYT1-F013.
     MOVE     PARA-HFROM          TO   JYT1-F011.
* 受信日順
     START  BMSJYTF1 KEY  IS  >=  JYT1-F013   JYT1-F011
*
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
     GO  TO    INSTART-EXIT.
*
 INSTART-200.
     MOVE     PARA-TOKCD          TO   JYT2-F013.
     MOVE     PARA-HFROM          TO   JYT2-F329.
* 計上日順
     START  BMSJYTF2 KEY  IS  >=  JYT2-F013   JYT2-F329
*
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
     GO  TO    INSTART-EXIT.
 INSTART-EXIT.
     EXIT.
*
****************************************************************
*   受領訂正メッセージ読込み
****************************************************************
 INREAD-SEC                 SECTION.
*
     MOVE    "INREAD-SEC"   TO   S-NAME.
*
     IF  PARA-HSYU  =  "2"
         GO  TO   INREAD-200
     END-IF.
     READ     BMSJYTF1   NEXT
         AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   INREAD-EXIT
     END-READ.
     ADD     1    TO   READ-CNT.
     MOVE    JYT1-REC   TO   WJYT-REC.
     GO  TO    INREAD-EXIT.
*
 INREAD-200.
     READ     BMSJYTF2   NEXT
         AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   INREAD-EXIT
     END-READ.
     ADD     1    TO   READ-CNT.
     MOVE    JYT2-REC   TO   WJYT-REC.
*
 INREAD-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
     IF   PARA-HSYU  =  "2"
          GO  TO   MAIN-200
     END-IF.
* 受信日
**  抽出条件判定
     IF   WJYT-F013 >  PARA-TOKCD
          MOVE   "END"   TO   END-FLG
              GO  TO   MAIN-EXIT
     END-IF.
*対象期間終了
     IF  WJYT-F011  >  PARA-HTO
          MOVE   "END"   TO   END-FLG
              GO  TO   MAIN-EXIT
     END-IF.
*受領訂正ワーク出力
*
     PERFORM  JTW-WRITE-SEC.
     GO  TO    MAIN-900.
 MAIN-200.
* 計上日
**  抽出条件判定
     IF   WJYT-F013 >  PARA-TOKCD
          MOVE   "END"   TO   END-FLG
              GO  TO   MAIN-EXIT
     END-IF.
*対象期間終了
     IF  WJYT-F329  >  PARA-HTO
          MOVE   "END"   TO   END-FLG
              GO  TO   MAIN-EXIT
     END-IF.
*
*受領訂正ワーク出力
*
     PERFORM  JTW-WRITE-SEC.
*
     GO  TO    MAIN-900.
 MAIN-900.
     PERFORM  INREAD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*件数出力
*受領訂正読込
     DISPLAY "BMSJYTF   READ CNT = " READ-CNT UPON CONS.
*受領訂正ワ－ク出力
     DISPLAY "BMSJYTW  WRITE CNT = " WRITE-CNT UPON CONS.
*
     DISPLAY "BMSJYRF  UNMCH CNT = " UNMACH-CNT UPON CONS.
     IF  PARA-HSYU  =  "1"
         CLOSE  BMSJYTF1
     END-IF.
     IF  PARA-HSYU  =  "2"
         CLOSE  BMSJYTF2
     END-IF.
*
     CLOSE     BMSJYTW   BMSJYRF  HTOKMS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 JTW-WRITE-SEC         SECTION.
*
     MOVE    "JTW-WRITE^SEC"     TO   S-NAME.
*
     MOVE     SPACE               TO   JTW-REC.
     INITIALIZE                        JTW-REC.
     MOVE     WJYT-F011      TO  JTW-F011.
     MOVE     WJYT-F012      TO  JTW-F012.
     MOVE     WJYT-F013      TO  JTW-F013.
*
     PERFORM  TOK-READ.
     MOVE     TOK-F02        TO  JTW-F02.
     MOVE     WJYT-F302      TO  JTW-F03.
     MOVE     WJYT-F309      TO  JTW-F04.
     MOVE     WJYT-F311      TO  JTW-F05.
     MOVE     WJYT-F312      TO  JTW-F06.
     MOVE     WJYT-F329      TO  JTW-F07.
     MOVE     WJYT-F337      TO  JTW-F08.
     MOVE     WJYT-F402      TO  JTW-F09.
     MOVE     SPACE          TO  JTW-F10.
     MOVE     WJYT-F412      TO  JTW-F11.
     MOVE     WJYT-F416      TO  JTW-F12.
     MOVE     WJYT-F419      TO  JTW-F13.
     MOVE     WJYT-F445      TO  JTW-F14.
     MOVE     WJYT-F446      TO  JTW-F15.
     MOVE     WJYT-F447      TO  JTW-F16.
     MOVE     WJYT-F448      TO  JTW-F17.
*
     PERFORM  BMSJYR-READ.
     IF   JYR-INVSW  =  SPACE
          MOVE     JYR-F455       TO  JTW-F18
     ELSE
          ADD   1            TO  UNMACH-CNT
     END-IF.
     MOVE     WJYT-F450      TO  JTW-F19.
     MOVE     SPACE          TO  JTW-F20.
*#2019/09/20 NAV ST
     IF       WJYT-F344  NOT NUMERIC
              MOVE ZERO      TO  JTW-F21
     ELSE
              MOVE WJYT-F344 TO  JTW-F21
     END-IF.
*#2019/09/20 NAV ED
     MOVE     WJYT-FIL5      TO  JTW-F99.
*
     WRITE   JTW-REC.
     ADD     1      TO      WRITE-CNT.
*
 JTW-WRITE-EXIT.
     EXIT.
*************************************************
 TOK-READ         SECTION.
** 取引先マスタ
     MOVE    WJYT-F013      TO   TOK-F01.
     READ  HTOKMS
       INVALID
         MOVE  ALL  SPACE        TO  TOK-F02
     END-READ.
*
 TOK-READ-EXIT.
     EXIT.
************************************************
 BMSJYR-READ      SECTION.
** 受領マスタ
     MOVE    WJYT-F013      TO   JYR-F013.
     MOVE    WJYT-F302      TO   JYR-F302.
     MOVE    WJYT-F402      TO   JYR-F402.
     MOVE    SPACE          TO   JYR-INVSW.
*
     READ  BMSJYRF
       INVALID
         MOVE  ZERO              TO  JYR-F455
         MOVE  "INV"             TO  JYR-INVSW
     END-READ.
*
 BMSJYR-READ-EXIT.
     EXIT.

****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    サブシステム　　　　：　ＥＤＩＣシステム構築
*    業務名　　　　　　　：　ＥＤＩシステム出荷データ作成
*    モジュール名　　　　：　送信出荷データ編集　　　
*    作成日／更新日　　　：　2015/09/17
*    作成者／更新者　　　：　NAV TAKAHASIH
*    処理概要　　　　　　：　出荷送信対象の出荷データをワークに
*                            抽出する。（マキバ用）
*    更新履歴            ：
*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SED0091W.
 AUTHOR.                NAV.
 DATE-WRITTEN.          15/09/17.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*ＥＤＩＣ出荷ＭＳＧ
     SELECT   EDSYUKF   ASSIGN    TO        DA-01-VI-EDSYUKL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       SYK-F801
                                            SYK-F011  SYK-F012
                                            SYK-F013  SYK-F02
                                            SYK-F03   SYK-F04
                                            SYK-F05   SYK-F06
                        FILE      STATUS    SYK-STATUS.
*ＥＤＩＣ送信出荷ワーク
     SELECT   EWSYUKF   ASSIGN    TO        DA-01-VI-EWSYUKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SYW-F011  SYW-F012
                                            SYW-F013  SYW-F02
                                            SYW-F03   SYW-F04
                                            SYW-F05   SYW-F06
                        FILE      STATUS    SYW-STATUS.
*ＥＤＩＣ発注ＭＳＧ
     SELECT   EDJOHOF   ASSIGN    TO        DA-01-VI-EDJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JOH-F011  JOH-F012
                                            JOH-F013  JOH-F02
                                            JOH-F03   JOH-F04
                                            JOH-F05   JOH-F06
                        FILE      STATUS    JOH-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
*
******************************************************************
*    ＥＤＩＣ出荷ＭＳＧ
******************************************************************
 FD  EDSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     EDSYUKF   OF        XFDLIB
              JOINING   SYK       PREFIX.
*
******************************************************************
*    ＥＤＩＣ送信出荷ワーク
******************************************************************
 FD  EWSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     EWSYUKF   OF        XFDLIB
              JOINING   SYW       PREFIX.
*
******************************************************************
*    ＥＤＩＣ発注ＭＳＧ
******************************************************************
 FD  EDJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     EDJOHOF   OF        XFDLIB
              JOINING   JOH       PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*ワーク出荷情報データ
     COPY   EWSYUKF  OF XFDLIB  JOINING   WSY  AS   PREFIX.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  FILHED-CNT          PIC  9(08)     VALUE  ZERO.
     03  DHED1-CNT           PIC  9(08)     VALUE  ZERO.
     03  DHED2-CNT           PIC  9(08)     VALUE  ZERO.
     03  DMEI-CNT            PIC  9(08)     VALUE  ZERO.
     03  SND-CNT             PIC  9(08)     VALUE  ZERO.
     03  OUT-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  EDJOHOF-INV-FLG     PIC  X(03)     VALUE  SPACE.
 01  WK-SYK-F05              PIC  9(09)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*システム時刻の編集
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HHMMSS     PIC  9(06).
     03  SYS-MS         PIC  9(02).
*
 01  WK-ST.
     03  SYW-STATUS        PIC  X(02).
     03  JOH-STATUS        PIC  X(02).
     03  SYK-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SED0091W".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SED0091W".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SED0091W".
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
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-TOKCD             PIC   9(08).
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION  USING  PARA-TOKCD.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   EDSYUKF.
     MOVE      "EDSYUKL3"   TO   AB-FILE.
     MOVE      SYK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   EWSYUKF.
     MOVE      "EWSYUKL1"   TO   AB-FILE.
     MOVE      SYW-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   EDJOHOF.
     MOVE      "EDJOHOL2"   TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
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
*
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
*
 GENERAL-PROCESS-EXIT.
     EXIT.
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     OPEN     I-O       EDJOHOF  EDSYUKF.
     OPEN     I-O       EWSYUKF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
     MOVE     SPACE     TO        WK-INV-FLG.
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
*システム時刻の取得
     ACCEPT   SYS-TIME       FROM TIME.
*    ＥＤＩＣ出荷ＭＳＧデータ読込み
     PERFORM EDSYUKF-READ-SEC.
*
     IF   END-FLG  =  "END"
        DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
        DISPLAY NC"＃＃　送信対象のデータが存在　＃＃" UPON CONS
        DISPLAY NC"＃＃　しません。出荷データを　＃＃" UPON CONS
        DISPLAY NC"＃＃　確認して下さい。　　　　＃＃" UPON CONS
        DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
        MOVE  4000              TO   PROGRAM-STATUS
        STOP  RUN
     ELSE  *>↓１件目のデータはワークに保存する
        MOVE  SPACE             TO   WSY-REC
        INITIALIZE                   WSY-REC
        MOVE  SYK-REC           TO   WSY-REC
        MOVE  1                 TO   FILHED-CNT
        ADD   1                 TO   SND-CNT
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 EDSYUKF-READ-SEC    SECTION.
*
     READ     EDSYUKF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  EDSYUKF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
 READ-010.
*    送信済ＦＬＧチェック
     IF       SYK-F801  =  "1"
              MOVE     "END"        TO  END-FLG
              GO                    TO  EDSYUKF-READ-EXIT
     END-IF.
*    取引先ＣＤチェック
     IF       SYK-F013  =  PARA-TOKCD
              CONTINUE
     ELSE
              GO                    TO  EDSYUKF-READ-SEC
     END-IF.
*
 EDSYUKF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    ＥＤＩＣ出荷ＭＳＧをＥＤＩＣ送信出荷ワークにセット
     MOVE     SPACE               TO   SYW-REC.
     INITIALIZE                        SYW-REC.
*
     MOVE     SYK-REC             TO   SYW-REC.
*****DISPLAY "SYW-REC = " SYW-REC UPON CONS.
     IF  WK-SYK-F05  NOT =  SYK-F05
         ADD  1                   TO   DHED1-CNT
         ADD  1                   TO   DHED2-CNT
         ADD  2                   TO   SND-CNT
         MOVE SYK-F05             TO   WK-SYK-F05
     END-IF.
*
     WRITE  SYW-REC.
     ADD    1                     TO   OUT-CNT.
     IF  SYK-F06  NOT =  ZERO
         ADD  1                   TO   DMEI-CNT
         ADD  1                   TO   SND-CNT
     END-IF.
*ＥＤＩＣ出荷ＭＳＧデータ送信ＦＬＧ更新
     MOVE     "1"                 TO   SYK-F801.
     MOVE     SYS-DATEW           TO   SYK-F802.
     MOVE     SYS-HHMMSS          TO   SYK-F803.
     REWRITE  SYK-REC.
*ＥＤＩＣ発注ＭＳＧデータ送信ＦＬＧ更新
     MOVE     SYK-F011            TO   JOH-F011.
     MOVE     SYK-F012            TO   JOH-F012.
     MOVE     SYK-F013            TO   JOH-F013.
     MOVE     SYK-F02             TO   JOH-F02.
     MOVE     SYK-F03             TO   JOH-F03.
     MOVE     SYK-F04             TO   JOH-F04.
     MOVE     SYK-F05             TO   JOH-F05.
     MOVE     SYK-F06             TO   JOH-F06.
     PERFORM  EDJOHOF-READ-SEC
     IF   EDJOHOF-INV-FLG  =  SPACE
          MOVE     "1"            TO   JOH-F801
          MOVE     SYS-DATEW      TO   JOH-F802
          MOVE     SYS-HHMMSS     TO   JOH-F803
          REWRITE  JOH-REC
     END-IF.
*
 MAIN010.
*    ＥＤＩＣ発注ＭＳＧデータ読込み
     PERFORM EDSYUKF-READ-SEC.
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
     IF   FILHED-CNT  >  ZERO
          MOVE SPACE             TO   SYW-REC
          INITIALIZE                  SYW-REC
          MOVE WSY-REC           TO   SYW-REC
          MOVE ZERO              TO   SYW-F011  SYW-F012 SYW-F013
          MOVE SPACE             TO   SYW-F02
          MOVE ZERO              TO   SYW-F03  SYW-F04  SYW-F05
                                      SYW-F06
          MOVE  SND-CNT(3:6)     TO   SYW-F112
          MOVE  SYS-DATEW        TO   SYW-F110
          MOVE  SYS-HHMMSS       TO   SYW-F111
          WRITE SYW-REC
          ADD   1                TO   OUT-CNT
     END-IF.
*
     DISPLAY "EDSYUKF READ CNT     = " READ-CNT    UPON CONS.
     DISPLAY "FILHED       CNT     = " FILHED-CNT  UPON CONS.
     DISPLAY "DHED1        CNT     = " DHED1-CNT   UPON CONS.
     DISPLAY "DHED2        CNT     = " DHED2-CNT   UPON CONS.
     DISPLAY "DMEI         CNT     = " DMEI-CNT    UPON CONS.
     DISPLAY "SOUSIN       CNT     = " SND-CNT     UPON CONS.
     DISPLAY "OUTPUT       CNT     = " OUT-CNT     UPON CONS.
*
     CLOSE     EDSYUKF  EWSYUKF  EDJOHOF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　ＥＤＩＣ発注ＭＳＧファイル読込
****************************************************************
 EDJOHOF-READ-SEC    SECTION.
*
     READ     EDJOHOF    INVALID
              MOVE    "INV"       TO   EDJOHOF-INV-FLG
              NOT  INVALID
              MOVE    SPACE       TO   EDJOHOF-INV-FLG
     END-READ.
*
 EDJOHOF-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

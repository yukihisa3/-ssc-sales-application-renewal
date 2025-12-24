****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    サブシステム　　　　：　ＥＤＩＣシステム構築
*    業務名　　　　　　　：　ＥＤＩシステム出荷データ作成
*    モジュール名　　　　：　送信出荷データ作成　　　
*    作成日／更新日　　　：　2015/09/17
*    作成者／更新者　　　：　NAV TAKAHASIH
*    処理概要　　　　　　：　出荷送信フォーマットに変換する
*                            （マキバ用）
*    更新履歴            ：
*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SED0082B.
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
*ＥＤＩＣ送信出荷ワーク
     SELECT   EWSYUKF   ASSIGN    TO        DA-01-VI-EWSYUKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       SYW-F011  SYW-F012
                                            SYW-F013  SYW-F02
                                            SYW-F03   SYW-F04
                                            SYW-F05   SYW-F06
                        FILE      STATUS    SYW-STATUS.
*ＥＤＩＣ出荷送信データ
     SELECT   EDICSND   ASSIGN    TO        DA-01-S-EDICSND
                        FILE      STATUS    SND-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
*
******************************************************************
*    ＥＤＩＣ送信出荷ワーク
******************************************************************
 FD  EWSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     EWSYUKF   OF        XFDLIB
              JOINING   SYW       PREFIX.
*
******************************************************************
*    ＥＤＩＣ出荷送信データ
******************************************************************
 FD  EDICSND            BLOCK     CONTAINS  1   RECORDS
                        LABEL     RECORD   IS   STANDARD.
 01  SND-REC.
     03  SND-F01                  PIC   X(383).
     03  SND-F02                  PIC   X(001).
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  OUT-CNT             PIC  9(08)     VALUE  ZERO.
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
     03  SND-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SED0082B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SED0082B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SED0082B".
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
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
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
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   EDICSND.
     MOVE      "EDICSND "   TO   AB-FILE.
     MOVE      SND-STATUS   TO   AB-STS.
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
     OPEN     INPUT     EWSYUKF.
     OPEN     OUTPUT    EDICSND.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG.
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
*    ＥＤＩＣ送信出荷ワーク読込み
     PERFORM EWSYUKF-READ-SEC.
*
     IF   END-FLG  =  "END"
        DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
        DISPLAY NC"＃＃　送信対象のデータが存在　＃＃" UPON CONS
        DISPLAY NC"＃＃　しません。出荷データを　＃＃" UPON CONS
        DISPLAY NC"＃＃　確認して下さい。　　　　＃＃" UPON CONS
        DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
        MOVE  4000              TO   PROGRAM-STATUS
        STOP  RUN
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 EWSYUKF-READ-SEC    SECTION.
*
     READ     EWSYUKF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  EWSYUKF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
 EWSYUKF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    バッチ番号が０の場合
     IF   SYW-F05  =  ZERO
     AND  SYW-F06  =  ZERO  *>ファイルヘッダ出力
          MOVE   SPACE            TO   SND-REC
          INITIALIZE                   SND-REC
          MOVE   SYW-F100         TO   SND-REC
**********MOVE   "1"              TO   SND-F02
          WRITE  SND-REC
          ADD    1                TO   OUT-CNT
     ELSE
          IF  SYW-F05  NOT =  ZERO
          AND SYW-F06      =  ZERO
              MOVE   SPACE        TO   SND-REC  *>ヘッダ１
              INITIALIZE               SND-REC
              MOVE   SYW-F200     TO   SND-REC
**************MOVE   "1"          TO   SND-F02
              WRITE  SND-REC
              ADD    1            TO   OUT-CNT
              MOVE   SPACE        TO   SND-REC  *>ヘッダ２
              INITIALIZE               SND-REC
              MOVE   SYW-F300     TO   SND-REC
**************MOVE   "1"          TO   SND-F02
              WRITE  SND-REC
              ADD    1            TO   OUT-CNT
          ELSE
              MOVE   SPACE        TO   SND-REC  *>明細
              INITIALIZE               SND-REC
              MOVE   SYW-F400     TO   SND-REC
**************MOVE   "1"          TO   SND-F02
              WRITE  SND-REC
              ADD    1            TO   OUT-CNT
          END-IF
     END-IF.
*
 MAIN010.
*    ＥＤＩＣ送信出荷ワーク読込み
     PERFORM EWSYUKF-READ-SEC.
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
*
     DISPLAY "EWSYUKF READ CNT     = " READ-CNT    UPON CONS.
     DISPLAY "EDICSND OUT  CNT     = " OUT-CNT     UPON CONS.
*
     CLOSE    EWSYUKF  EDICSND.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

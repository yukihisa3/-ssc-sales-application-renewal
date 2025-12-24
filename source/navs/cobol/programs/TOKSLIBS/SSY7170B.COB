****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ジョイフル本田　仕入実績管理　　　*
*    業務名　　　　　　　：　受信仕入実績データ累積　　　　　　*
*    モジュール名　　　　：　受信仕入実績データ累積　　　　　　*
*    作成日／更新日　　　：　05/08/16                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受信した仕入実績データの累積を行 *
*                            なう。　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY7170B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          05/08/16.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*変換伝票データ
     SELECT   JDJISKF   ASSIGN    TO        DA-01-VI-JDJISKL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HEN-F01   HEN-F02
                                            HEN-F18   HEN-F27
                                            HEN-F08
                        FILE  STATUS   IS   HEN-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    変換伝票データ　ＲＬ＝300
*****************************************************************
 FD  JDJISKF
                        BLOCK CONTAINS      13  RECORDS
                        LABEL RECORD   IS   STANDARD.
     COPY     JDJISKF   OF        XFDLIB
              JOINING   HEN  AS   PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  HEN-CNT                 PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU               PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU-M1            PIC  9(08)     VALUE  ZERO.
 01  INV-FLG                 PIC  9(01)     VALUE  ZERO.
*
*ファイルヘッダレコード退避ワーク
 01  WK-DEPF-REC.
     03  WK-DEPF01          PIC  X(02).
     03  WK-DEPF02          PIC  X(02).
     03  WK-DEPF03          PIC  9(05).
     03  WK-DEPF04          PIC  9(08).
     03  WK-DEPF05          PIC  9(06).
     03  WK-DEPF06          PIC  9(08).
     03  WK-DEPF07          PIC  9(06).
     03  WK-DEPF08          PIC  9(05).
     03  WK-DEPF09          PIC  X(01).
     03  WK-DEPF10          PIC  X(05).
     03  WK-DEPF11          PIC  X(01).
     03  WK-DEPF12          PIC  X(79).
*ヘッダレコード退避ワーク
 01  WK-DEPH-REC.
     03  WK-DEPH01          PIC  X(02).
     03  WK-DEPH02          PIC  X(02).
     03  WK-DEPH03          PIC  9(05).
     03  WK-DEPH04          PIC  X(02).
     03  WK-DEPH05          PIC  X(05).
     03  WK-DEPH06          PIC  X(01).
     03  WK-DEPH07          PIC  X(02).
     03  WK-DEPH08          PIC  X(04).
     03  WK-DEPH09          PIC  X(10).
     03  WK-DEPH10          PIC  X(04).
     03  WK-DEPH11          PIC  X(04).
     03  WK-DEPH12          PIC  X(01).
     03  WK-DEPH13          PIC  X(07).
     03  WK-DEPH14          PIC  X(07).
     03  WK-DEPH15          PIC  S9(09)V9(02).
     03  WK-DEPH16          PIC  S9(09)V9(02).
     03  WK-DEPH17          PIC  9(08).
     03  WK-DEPH18          PIC  X(42).
*    明細レコード退避ワーク
 01  WK-DEPM-REC.
     03  WK-DEPM01          PIC  X(02).
     03  WK-DEPM02          PIC  X(02).
     03  WK-DEPM03          PIC  9(05).
     03  WK-DEPM04          PIC  9(02).
     03  WK-DEPM05          PIC  9(02).
     03  WK-DEPM06          PIC  X(01).
     03  WK-DEPM07          PIC  X(13).
     03  WK-DEPM08          PIC  S9(06)V9(02).
     03  WK-DEPM09          PIC  S9(08)V9(02).
     03  WK-DEPM10          PIC  S9(08)V9(02).
     03  WK-DEPM11          PIC  X(73).
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  HEN-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY7170B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7170B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7170B".
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
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JDJISKF.
     MOVE      "JDJISKF"   TO    AB-FILE.
     MOVE      HEN-STATUS  TO    AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000        TO    PROGRAM-STATUS.
     STOP      RUN.
*
*
 END     DECLARATIVES.
****************************************************************
*                                                              *
****************************************************************
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
     OPEN     I-O       JDJISKF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    HEN-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT   INV-FLG.
*
     MOVE     SPACE     TO        HEN-REC.
     INITIALIZE                   HEN-REC.
     MOVE     20060920  TO        HEN-F01.
     START  JDJISKF  KEY  IS  >=  HEN-F01  HEN-F02  HEN-F18
                                  HEN-F27  HEN-F08
            INVALID
            MOVE    9   TO        END-FG
            DISPLAY NC"対象データ無し" UPON CONS
            GO          TO        INIT-EXIT
     END-START.
**************
*システム日付*
**************
     ACCEPT   SYS-DATE  FROM      DATE.
*
     PERFORM JDJISKF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO    S-NAME.
*
     MOVE     SPACE              TO    HEN-F99.
     REWRITE  HEN-REC.
     ADD      1                  TO    HEN-CNT.
*
     PERFORM  JDJISKF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 JDJISKF-READ-SEC    SECTION.
*
     READ   JDJISKF  NEXT  AT  END
            MOVE     9     TO    END-FG
            GO             TO    JDJISKF-READ-EXIT
     END-READ.
*
     ADD    1              TO    RD-CNT.
*
 JDJISKF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      HEN-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     JDJISKF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

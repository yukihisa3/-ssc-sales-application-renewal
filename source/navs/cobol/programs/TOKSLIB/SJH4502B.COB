****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　変換伝票データ作成（支払データ）　*
*    作成日／更新日　　　：　2002/10/25                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　オンラインデータより支払データの　*
*                            抽出を行なう。　　　　　　　　　　*
*                            イオン（大阪）                    *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SJH4502B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          02/10/25.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受信データファイル
     SELECT   CVCSG001  ASSIGN    TO        DA-01-S-CVCSG001
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    DEN-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*支払明細データ
     SELECT   IONSHI    ASSIGN    TO        DA-01-S-IONSHI
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    ION-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受信データ　ＲＬ＝　２５６　ＢＦ＝　１
******************************************************************
 FD  CVCSG001
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  DEN-REC.
     03  DEN-01                  OCCURS 2.
         05  DEN-01A             PIC  X(03).
         05  DEN-02A             PIC  X(13).
         05  DEN-03A             PIC  X(02).
         05  DEN-04A             PIC  X(110).
*
******************************************************************
*    分割データ　ＲＬ＝　１２８　ＢＦ＝　１
******************************************************************
 FD  IONSHI
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  ION-REC.
     03  ION-F01                 PIC  X(128).
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  FLG-OK                  PIC  X(02)     VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  ION-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJH4502B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJH4502B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJH4502B".
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
*取引先コード変換
 01  WK-TORICD             PIC  X(05).
 01  WK-TORICD-R           REDEFINES   WK-TORICD.
     03  WK-TORICD-H       PIC  9(05).
*伝票番号変換
 01  WK-DENNO              PIC  X(07).
 01  WK-DENNO-R            REDEFINES   WK-DENNO.
     03  WK-DENNO-H        PIC  9(07).
*店舗コード変換
 01  WK-TENPO              PIC X(04).
 01  WK-TENPO-R            REDEFINES   WK-TENPO.
     03  WK-TENPO-H        PIC 9(04).
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
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CVCSG001.
     MOVE      "CVCSG001"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   IONSHI.
     MOVE      "IONSHI  "   TO   AB-FILE.
     MOVE      ION-STATUS   TO   AB-STS.
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
     STOP     RUN.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     CVCSG001.
     OPEN     OUTPUT    IONSHI.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
     MOVE     SPACE     TO        FLG-OK.
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
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 CVCS-READ    SECTION.
*
     READ     CVCSG001
              AT END
              MOVE      9                   TO  END-FG
              GO                            TO  CVCS-READ-EXIT
              NOT AT END
              ADD       1                   TO  RD-CNT
     END-READ.
*
     PERFORM  VARYING   IDX      FROM      1  BY  1
              UNTIL     IDX      >         2
*********** IF    DEN-01A(IDX)  =    "JET"
*********** AND   DEN-03A(IDX)  =    "04"
***********   DISPLAY "DEN-01A(IDX) = " DEN-01A(IDX) UPON CONS
***********   DISPLAY "DEN-03A(IDX) = " DEN-03A(IDX) UPON CONS
***********   DISPLAY "FLG-OK       = " FLG-OK       UPON CONS
*********** END-IF
        IF    DEN-01A(IDX)  =    "JET"
        AND   DEN-03A(IDX)  =    "04"
        AND   FLG-OK        =    "ED"
************  IF   IDX = 2
************       CONTINUE
************  ELSE
************       MOVE  SPACE        TO   ION-REC
************       INITIALIZE              ION-REC
************       MOVE  DEN-01(IDX)  TO   ION-REC
************DISPLAY "IDX    = " IDX         UPON CONS
************DISPLAY "DEN-01 = " DEN-01(IDX) UPON CONS
************       WRITE ION-REC
************       ADD   1            TO   WRT-CNT
************  END-IF
              MOVE   "OK"             TO   FLG-OK
        ELSE
              IF    DEN-01A(IDX)  =    "JET"
                    MOVE   "ED"       TO   FLG-OK
              END-IF
              IF   FLG-OK  =  "OK"
                   MOVE  SPACE        TO   ION-REC
                   INITIALIZE              ION-REC
                   MOVE  DEN-01(IDX)  TO   ION-REC
                   WRITE ION-REC
                   ADD   1            TO   WRT-CNT
              END-IF
        END-IF
*
     END-PERFORM.
*
 CVCS-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
     PERFORM  CVCS-READ.
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
     CLOSE     CVCSG001  IONSHI.
*
     DISPLAY "READ-CNT  = "  RD-CNT  UPON CONS.
     DISPLAY "WRT-CNT   = "  WRT-CNT UPON CONS.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

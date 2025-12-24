****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷業務　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ＬＩＮＫＳ連携処理                *
*    モジュール名　　　　：　連携オンライン状況ファイル削除作成*
*    作成日／更新日　　　：　2013/02/18                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　受け取った各パラメタにて、連携オ  *
*                            ンライン状況Ｆを読み、条件に合致  *
*                            したレコードを削除する。          *
*                            削除後、再度、連携状況再更新ファ　*
*                            イルを作成する。　　　　　　　　　*
*　　更新日／更新者　　　：                                    *
*　                                                            *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SBT0930B.
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
*連携状況再更新ファイル
     SELECT   LNKSAIF   ASSIGN    TO        DA-01-VI-LNKSAIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       SAI-F01   SAI-F02
                                            SAI-F03
                        FILE      STATUS    SAI-STATUS.
*連携オンライン状況ファイル
     SELECT   LNKONLF   ASSIGN    TO        DA-01-VI-LNKONLL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       ONL-F01   ONL-F02
                                            ONL-F03   ONL-F04
                                            ONL-F05
                        FILE      STATUS    ONL-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    連携状況再更新ファイル
******************************************************************
 FD  LNKSAIF            LABEL RECORD   IS   STANDARD.
     COPY     LNKSAIF   OF        XFDLIB
              JOINING   SAI       PREFIX.
******************************************************************
*    連携オンライン状況ファイル
******************************************************************
 FD  LNKONLF            LABEL RECORD   IS   STANDARD.
     COPY     LNKONLF   OF        XFDLIB
              JOINING   ONL       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  DEL-CNT             PIC  9(08)     VALUE  ZERO.
     03  DEL-CNT1            PIC  9(08)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  SAI-STATUS        PIC  X(02).
     03  ONL-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SBT0930B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SBT0930B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SBT0930B".
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
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   LNKSAIF.
     MOVE      "LNKSAIL1"   TO   AB-FILE.
     MOVE      SAI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   LNKONLF.
     MOVE      "LNKONLL1"   TO   AB-FILE.
     MOVE      ONL-STATUS   TO   AB-STS.
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
     OPEN     INPUT     LNKSAIF.
     OPEN     I-O       LNKONLF.
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
*    連携状況再更新ファイル
     PERFORM LNKSAIF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 LNKSAIF-READ-SEC    SECTION.
*
     READ     LNKSAIF
              AT  END
                  MOVE     "END"    TO  END-FLG
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
 LNKSAIF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    出荷状況管理ファイルスタート
     MOVE     ZERO                TO   DEL-CNT1.
     MOVE     SPACE               TO   ONL-REC.
     INITIALIZE                        ONL-REC.
     MOVE     SAI-F01             TO   ONL-F01.
     MOVE     SAI-F02             TO   ONL-F02.
     MOVE     SAI-F03             TO   ONL-F03.
     START LNKONLF KEY IS >=   ONL-F01  ONL-F02  ONL-F03
                               ONL-F04  ONL-F05
           INVALID
              DISPLAY NC"＃削除対象無！！" UPON CONS
              DISPLAY "#NO.=" SAI-F01 "-" SAI-F02 "-" SAI-F03
                       " #"  UPON CONS
              GO                  TO   MAIN015
     END-START.
*
 MAIN000.
     READ  LNKONLF  NEXT  AT  END
           GO                     TO    MAIN015
     END-READ.
*
     IF    SAI-F01  =  ONL-F01
     AND   SAI-F02  =  ONL-F02
     AND   SAI-F03  =  ONL-F03
           DELETE   LNKONLF
           ADD      1             TO   DEL-CNT
           ADD      1             TO   DEL-CNT1
           GO                     TO   MAIN000
     END-IF.
 MAIN015.
*****IF    DEL-CNT1  >  ZERO
***********連携オンライン状況ファイル再計算
           CALL    "SBT0940B" USING  SAI-F01  SAI-F02  SAI-F03.
*****END-IF.
 MAIN020.
*    出荷状況管理ファイル
     PERFORM LNKSAIF-READ-SEC.
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
     DISPLAY "LNKSAIF READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "LNKONLF DELE CNT = " DEL-CNT   UPON CONS.
*
     CLOSE     LNKSAIF  LNKONLF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

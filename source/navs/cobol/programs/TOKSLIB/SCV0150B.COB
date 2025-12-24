****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＣＶＣＳ管理                      *
*    モジュール名　　　　：　パラメタ作成                      *
*    作成日／更新日　　　：　99/10/07                          *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　受信パラメタＦを読み、パラメタを作*
*                            成する。                          *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SCV0150B.
 AUTHOR.               TAKAHASHI.
 DATE-WRITTEN.         99/10/07.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*受信パラマタファイル
     SELECT  PARAFILE  ASSIGN    TO        DA-01-S-PARAFILE
                       FILE      STATUS    PAR-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 受信パラメタファイル                               *
****************************************************************
 FD  PARAFILE          BLOCK     CONTAINS  1    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      PARAINS1  OF   XFDLIB
                       JOINING   PAR       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  PAR-ST                   PIC  X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  PAR-ERR           PIC N(15) VALUE
         NC"パラメタファイルエラー".
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
***  エラーファイル名
 01  ERR-FILE.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-FILE   => ".
     03  E-FILE                   PIC  X(08).
***  エラーステータス名
 01  ERR-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-STATUS => ".
     03  E-ST                     PIC  9(02).
*------------------------------------------------------------*
 LINKAGE              SECTION.
*------------------------------------------------------------*
*パラメタ取得
 01  LINK-JDATE            PIC 9(08).
 01  LINK-JTIME            PIC 9(04).
 01  LINK-JTOKCD           PIC 9(08).
 01  LINK-JPASS            PIC X(13).
 01  LINK-JHENID           PIC X(08).
 01  LINK-JSEN             PIC X(01).
 01  LINK-JSENY            PIC X(01).
 01  LINK-JSENLIB          PIC X(08).
 01  LINK-JSYUSINF         PIC X(08).
 01  LINK-JKJOB            PIC X(08).
 01  LINK-JKEKA            PIC 9(02).
 01  LINK-JTIMES           PIC 9(04).
 01  LINK-JTIMEE           PIC 9(04).
 01  LINK-JRL              PIC 9(04).
 01  LINK-JTOKNM           PIC X(15).
*
**************************************************************
 PROCEDURE             DIVISION   USING    LINK-JDATE
                                           LINK-JTIME
                                           LINK-JTOKCD
                                           LINK-JPASS
                                           LINK-JHENID
                                           LINK-JSEN
                                           LINK-JSENY
                                           LINK-JSENLIB
                                           LINK-JSYUSINF
                                           LINK-JKJOB
                                           LINK-JKEKA
                                           LINK-JTIMES
                                           LINK-JTIMEE
                                           LINK-JRL
                                           LINK-JTOKNM.
**************************************************************
 DECLARATIVES.
 PAR-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PARAFILE.
     MOVE        PAR-ST    TO        E-ST.
     MOVE        "PARAFILE" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     PAR-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC.
     PERFORM   END-SEC.
     STOP  RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               0.0       *
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"     TO   S-NAME.
*ファイルのＯＰＥＮ
     OPEN      INPUT  PARAFILE.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*パラメタファイル読込み
 MAIN010.
     READ  PARAFILE  AT  END
           DISPLAY "## ﾊﾟﾗﾒﾀﾌｧｲﾙ READ ｲｼﾞｮｳ##" UPON CONS
           MOVE    "4000"    TO   PROGRAM-STATUS
           GO                TO   MAIN-EXIT
     END-READ.
*受取ったパラメタをファイル項目へセット
     MOVE      PAR-F01       TO   LINK-JDATE.
     MOVE      PAR-F02       TO   LINK-JTIME.
     MOVE      PAR-F03       TO   LINK-JTOKCD.
     MOVE      PAR-F04       TO   LINK-JPASS.
     MOVE      PAR-F05       TO   LINK-JHENID.
     MOVE      PAR-F06       TO   LINK-JSEN.
     MOVE      PAR-F07       TO   LINK-JSENY.
     MOVE      PAR-F08       TO   LINK-JSENLIB.
     MOVE      PAR-F09       TO   LINK-JSYUSINF.
     MOVE      PAR-F10       TO   LINK-JKJOB.
     MOVE      PAR-F11       TO   LINK-JKEKA.
     MOVE      PAR-F12       TO   LINK-JTIMES
     MOVE      PAR-F13       TO   LINK-JTIMEE
     MOVE      PAR-F14       TO   LINK-JRL.
     MOVE      PAR-F15       TO   LINK-JTOKNM.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"      TO   S-NAME.
*ファイルのＯＰＥＮ
     CLOSE     PARAFILE.
*
 END-EXIT.
     EXIT.
*****************<<  SCV0100B   END PROGRAM  >>******************

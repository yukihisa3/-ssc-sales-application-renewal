****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＣＶＣＳ管理                      *
*    モジュール名　　　　：　受信パラメタファイル作成          *
*    作成日／更新日　　　：　99/10/06                          *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　受取ったパラメタより受信パラメタＦ*
*                            を作成する。                      *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SCV0100B.
 AUTHOR.               TAKAHASHI.
 DATE-WRITTEN.         99/10/06.
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
                       FILE   STATUS   IS  PAR-ST.
*取引先マスタ
     SELECT   TOKMS2   ASSIGN    TO        DA-01-VI-TOKMS2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TOK-F01
                       FILE  STATUS    IS  TOK-ST.
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
******************************************************************
*    取引先マスタ
******************************************************************
 FD  TOKMS2            LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  PAR-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  PAR-ERR           PIC N(15) VALUE
         NC"パラメタファイルエラー".
     03  TOK-ERR           PIC N(15) VALUE
         NC"取引先マスタエラー".
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
 01  LINK-JRL              PIC X(04).
 01  LINK-JYUKBN           PIC X(01).
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
                                           LINK-JRL
                                           LINK-JYUKBN.
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
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE TOKMS2.
     MOVE        TOK-ST    TO        E-ST.
     MOVE        "TOKMS2"   TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TOK-ERR   UPON      CONS.
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
     OPEN      INPUT   TOKMS2.
     OPEN      OUTPUT  PARAFILE.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*受取ったパラメタをファイル項目へセット
     MOVE      SPACE         TO   PAR-REC.
     INITIALIZE                   PAR-REC.
     MOVE      LINK-JDATE    TO   PAR-F01.
     MOVE      LINK-JTIME    TO   PAR-F02.
     MOVE      LINK-JTOKCD   TO   PAR-F03.
     MOVE      LINK-JPASS    TO   PAR-F04.
     MOVE      LINK-JHENID   TO   PAR-F05.
     MOVE      LINK-JSEN     TO   PAR-F06.
     MOVE      LINK-JSENY    TO   PAR-F07.
     MOVE      LINK-JSENLIB  TO   PAR-F08.
     MOVE      LINK-JSYUSINF TO   PAR-F09.
     MOVE      LINK-JKJOB    TO   PAR-F10.
     MOVE      LINK-JRL      TO   PAR-F14.
*取引先マスタより取引先名カナの取得
     MOVE      PAR-F03       TO   TOK-F01.
     READ      TOKMS2
               INVALID
               MOVE ALL "*"  TO   PAR-F15
               NOT  INVALID
               MOVE TOK-F04  TO   PAR-F15
     END-READ.
     MOVE      LINK-JYUKBN   TO   PAR-F16.
*パラメタファイル更新
     WRITE     PAR-REC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"      TO   S-NAME.
*ファイルのＯＰＥＮ
     CLOSE     TOKMS2  PARAFILE.
*
 END-EXIT.
     EXIT.
*****************<<  SCV0100B   END PROGRAM  >>******************

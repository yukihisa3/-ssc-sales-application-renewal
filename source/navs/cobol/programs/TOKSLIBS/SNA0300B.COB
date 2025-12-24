**************************************************************
*                                                            *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　*
*    サブシステム　　　　：　ＨＧ苗業務連携                  *
*    業務名　　　　　　　：　                                *
*    モジュール名　　　　：　HULFT疎通確認処理               *
*    作成日／更新日　　　：　12/04/12                        *
*    作成者／更新者　　　：　ＮＡＶ三浦　                    *
*    処理概要　　　　　　：　HULFT排他制御ファイルが疎通済み *
*                            になるまで、１分間監視する。    *
*                                                            *
**************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNA0300B.
 AUTHOR.               MIURA.
 DATE-WRITTEN.         12/04/12.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*
*排他制御ファイル
     SELECT  NARLOCF2  ASSIGN TO    NARLOCF2
             ORGANIZAITION         SEQUENTIAL
             ACCESS      MODE      SEQUENTIAL
                         FILE      STATUS    NAR-ST.
*
****************************************************************
 DATA                DIVISION.
*************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 排他制御ファイル　　　　　　　　　             *
****************************************************************
 FD  NARLOCF2           LABEL     RECORD    IS   STANDARD.
                       COPY      NARLOCF2   OF   XFDLIB
                       JOINING   NAR       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
 01  HZK.
     03  YMD01                 PIC  9(02)     VALUE   20.
     03  YMD02                 PIC  9(06)     VALUE   ZERO.
 01  JIKAN.
     03  JIKAN01               PIC  9(02)     VALUE   ZERO.
     03  JIKAN02               PIC  9(02)     VALUE   ZERO.
     03  JIKAN03               PIC  9(02)     VALUE   ZERO.
 01  WK-JIKAN.
     03  WK-JIKAN01            PIC  9(02)     VALUE   ZERO.
     03  WK-JIKAN02            PIC  9(02)     VALUE   ZERO.
     03  WK-JIKAN03            PIC  9(02)     VALUE   ZERO.
 01  S-JIKAN                   PIC  9(03)     VALUE   ZERO.
 01  K-JIKAN                   PIC  9(03)     VALUE   ZERO.
 01  END-FLG                   PIC  9(01)     VALUE   ZERO.
*ステータス領域
 01  STATUS-AREA.
     03  NAR-ST                PIC  X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  NAR-ERR           PIC N(13) VALUE
         NC"排他制御ファイル　　エラー".
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
****************************************************************
 LINKAGE              SECTION.
****************************************************************
* パラメーター エリア
 01  PARA-KEKKA                   PIC  X(02).
*
**************************************************************
 PROCEDURE         DIVISION   USING  PARA-KEKKA.
**************************************************************
 DECLARATIVES.
 FILEERR-SEC1              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NARLOCF2.
     MOVE        NAR-ST     TO       E-ST.
     MOVE       "NARLOCF2 "  TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     NAR-ERR   UPON      CONS.
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
     MOVE     "NG"           TO   PARA-KEKKA.
*ファイルのＯＰＥＮ
     OPEN      I-O          NARLOCF2.
     ACCEPT    YMD02        FROM   DATE.
     ACCEPT    WK-JIKAN     FROM   TIME.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*-------排他制御ファイルの読み込み
     READ  NARLOCF2  AT   END
           GO TO    MAIN-EXIT
     END-READ.
     IF  NAR-F06   =   1
         MOVE  SPACE              TO   NAR-F06
         MOVE  HZK                TO   NAR-F98
         MOVE  WK-JIKAN           TO   NAR-F99
         REWRITE NAR-REC
         MOVE  "OK"               TO   PARA-KEKKA
         GO TO    MAIN-EXIT
     ELSE
         CLOSE     NARLOCF2
         PERFORM   SUB-SEC
         OPEN      I-O     NARLOCF2
*----１分間監視　　　　　　　　
         IF  END-FLG  =  0
             GO TO     MAIN-SEC
         END-IF
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             サブ処理                               1.1       *
****************************************************************
 SUB-SEC              SECTION.
     MOVE     "SUB-SEC"     TO   S-NAME.
     ACCEPT    JIKAN     FROM   TIME.
*----１分間監視　　　　　　　　
     IF  JIKAN02  =  WK-JIKAN02
         COMPUTE  S-JIKAN  =  JIKAN03  - WK-JIKAN03
     ELSE
         COMPUTE  S-JIKAN  =  JIKAN03 + 60 - WK-JIKAN03
     END-IF.
     IF  S-JIKAN  <  60
         COMPUTE  K-JIKAN  =  S-JIKAN  - K-JIKAN
         IF K-JIKAN  >  1
             MOVE   ZERO     TO  K-JIKAN
             GO TO    SUB-EXIT
         ELSE
             GO TO    SUB-SEC
         END-IF
     ELSE
         MOVE   1  TO      END-FLG
     END-IF.
*
 SUB-EXIT.
     EXIT.
**************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*
     MOVE     "END-SEC"      TO   S-NAME.
*
*ファイルのCLOSE処理
     CLOSE     NARLOCF2.
*
 END-EXIT.
     EXIT.

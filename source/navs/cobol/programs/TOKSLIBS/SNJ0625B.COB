****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＥＤＩ受信サーバ　　　　　　　　　*
*    業務名　　　　　　　：　                                  *
*    モジュール名　　　　：　待受けＪＯＢ振分ＰＧ（ＢＭＳ追加）*
*    作成日／更新日　　　：　12/11/19                          *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　データ区分・受配信区分をパラメタと*
*                            して受取り、起動ＰＧＦＬＧを設定し*
*                            パラメタ出力する。                *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNJ0625B.
 AUTHOR.               OONO.
 DATE-WRITTEN.         XX/XX/XX.
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
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
***  エラーセクション名
 01  SEC-NAME.
     03  PGID              PIC  X(08) VALUE "SNJ0625B".
     03  S-NAME            PIC  X(20).
*------------------------------------------------------------*
 LINKAGE              SECTION.
*------------------------------------------------------------*
*パラメタ取得
 01  LINK-DTKBN            PIC X(02).
 01  LINK-JYUKBN           PIC X(01).
 01  LINK-KPGFLG           PIC X(01).
 01  LINK-KEKKA            PIC X(01).
*
**************************************************************
 PROCEDURE             DIVISION   USING    LINK-DTKBN
                                           LINK-JYUKBN
                                           LINK-KPGFLG
                                           LINK-KEKKA.
**************************************************************
 DECLARATIVES.
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
*ＰＧ開始を画面に出力
     DISPLAY  "*** " PGID "開始 ***"  UPON  CONS.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*
*起動ＰＧＦＬＧの設定
     EVALUATE  LINK-DTKBN  ALSO  LINK-JYUKBN
       WHEN    "01"  ALSO  "1"
       WHEN    "01"  ALSO  "2"
               MOVE    "1"       TO    LINK-KPGFLG
               MOVE     SPACE    TO    LINK-KEKKA
       WHEN    "02"  ALSO  "1"
       WHEN    "02"  ALSO  "2"
       WHEN    "03"  ALSO  "1"
       WHEN    "03"  ALSO  "2"
       WHEN    "07"  ALSO  "1"
       WHEN    "07"  ALSO  "2"
       WHEN    "08"  ALSO  "1"
       WHEN    "08"  ALSO  "2"
               MOVE    "2"       TO    LINK-KPGFLG
               MOVE     SPACE    TO    LINK-KEKKA
       WHEN    "04"  ALSO  "1"
       WHEN    "04"  ALSO  "2"
       WHEN    "05"  ALSO  "1"
       WHEN    "05"  ALSO  "2"
       WHEN    "06"  ALSO  "1"
       WHEN    "06"  ALSO  "2"
               MOVE    "3"       TO    LINK-KPGFLG
               MOVE     SPACE    TO    LINK-KEKKA
       WHEN    "01"  ALSO  "3"
       WHEN    "09"  ALSO  "3"
       WHEN    "02"  ALSO  "3"
       WHEN    "03"  ALSO  "3"
       WHEN    "07"  ALSO  "3"
       WHEN    "08"  ALSO  "3"
               MOVE    "4"       TO    LINK-KPGFLG
               MOVE     SPACE    TO    LINK-KEKKA
       WHEN    "04"  ALSO  "3"
       WHEN    "05"  ALSO  "3"
       WHEN    "06"  ALSO  "3"
               MOVE    "5"       TO    LINK-KPGFLG
               MOVE     SPACE    TO    LINK-KEKKA
       WHEN    OTHER
               MOVE     SPACE    TO    LINK-KPGFLG
               MOVE    "9"       TO    LINK-KEKKA
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"      TO   S-NAME.
*
     DISPLAY  "*** " PGID "終了 ***"  UPON  CONS.
*
 END-EXIT.
     EXIT.
*****************<<  SNJ0625B   END PROGRAM  >>******************

# SBM9010B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBM9010B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　イオン流通ＢＭＳオンライン        *
*    業務名　　　　　　　：　                                  *
*    モジュール名　　　　：　累積データ削除（発注）
*    作成日／更新日　　　：　2012/11/19                        *
*    作成者／更新者　　　：　ＮＡＶ武井　                      *
*    処理概要　　　　　　：　発注メッセージファイル削除する　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SBM9010B.
 AUTHOR.               NAV.
 DATE-WRITTEN.         12/11/19.
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
*発注メッセージファイル
     SELECT  BMSHACF  ASSIGN TO    DA-01-VI-BMSHACL1
             ORGANIZAITION         INDEXED
             ACCESS      MODE      SEQUENTIAL
             RECORD      KEY       HAC-F011  HAC-F012
                                   HAC-F013  HAC-F02
                                   HAC-F308  HAC-F346
                                   HAC-F302  HAC-F402
                         FILE      STATUS    HAC-ST.
****************************************************************
 DATA                DIVISION.
*************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 発注メッセージファイル　　　　　　　　             *
****************************************************************
 FD  BMSHACF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      BMSHACF   OF   XFDLIB
                       JOINING   HAC       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
 01  END-FLG                   PIC  X(01)     VALUE   SPACE.
 01  SAKUJO-FLG                PIC  X(01)     VALUE   SPACE.
 01  INIT-FLG                  PIC  X(01)     VALUE   SPACE.
 01  DLT-CNT                   PIC  9(07)     VALUE   ZERO.
 01  READ-CNT                  PIC  9(07)     VALUE   ZERO.
*
*ステータス領域
 01  STATUS-AREA.
     03  HAC-ST                PIC  X(02).
*
 01  HAC-BR-KEY2.
     03  HAC-BR-KEY21         PIC  X(08).
     03  HAC-BR-KEY22         PIC  X(04).
     03  HAC-BR-KEY23         PIC  X(08).
 01  WK-BR-KEY2.
     03  WK-BR-KEY21          PIC  X(08).
     03  WK-BR-KEY22          PIC  X(04).
     03  WK-BR-KEY23          PIC  X(08).
*
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  HAC-ERR           PIC N(20) VALUE
         NC"発注メッセージ累積ファイルエラー".
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
 01  MSG-AREA.
*----- 対象データーがゼロ件のメッセージエリア
*
     03  MSG-KENSU1.
         05  KENSU1A PIC N(12) VALUE
         NC"＊＊＊＊＊＊＊＊＊＊＊＊".
         05  KENSU1B PIC N(12) VALUE
         NC"＊＊＊＊＊＊＊＊＊＊＊＊".
     03  MSG-KENSU2.
         05  KENSU2A PIC N(24) VALUE
         NC"＊　指定された連携ＮＯは存在しません。　　　　＊".
     03  MSG-KENSU3.
         05  KENSU3A PIC N(11) VALUE NC"＊　　　連携ＮＯ　＝　".
*I       05  RENKEI-NO-FROM PIC X(09) VALUE SPACE.
         05  RENKEI-NO-FROM PIC X(10) VALUE SPACE.
         05  KENSU3B PIC N(08) VALUE NC"　　　　　　　＊".
     03  MSG-KENSU4.
         05  KENSU4A PIC N(24) VALUE
         NC"＊　指定された連携ＮＯは削除できない状態です　＊".
     03  MSG-KENSU5.
         05  KENSU5A PIC  N(10) VALUE NC"＊　　　　状態　＝　".
         05  JOUTAI-CD   PIC  9(01)  VALUE ZERO.
         05  JOUTAI-MEI  PIC  N(08)  VALUE SPACE.
         05  KENSU5B     PIC  N(05)  VALUE NC"　　　　＊".
     03  MSG-KENSU6A.
         05  MSG-KN6A1 PIC N(01) VALUE  NC"「".
         05  MSG-KN6A2 PIC N(25) VALUE  ALL  NC"＊".
         05  MSG-KN6A3 PIC N(01) VALUE  NC"」".
     03  MSG-KENSU6.
         05  MSG-KN61  PIC N(18) VALUE
         NC"「＊発注メッセージ　　削除件数　＝　".
         05  SAKUJO-RUISEKI  PIC 9(08).
         05  MSG-KN63  PIC    N(05) VALUE NC"　件　＊」".
*I↓
     03  MSG-KENSU21.
         05  KENSU21A PIC N(24) VALUE
         NC"＊　指定された連携ＮＯは、　　　　　　　　　　＊".
     03  MSG-KENSU22.
         05  KENSU22A PIC N(24) VALUE
         NC"＊　累積ファイルに存在しません。　　　　　　　＊".
*I↑
*--- 状態表示
     03  JOUTAI1    PIC    N(08) VALUE NC"（データ抽出済）".
     03  JOUTAI2    PIC    N(08) VALUE NC"（苗業務取込済）".
     03  JOUTAI3    PIC    N(08) VALUE NC"（取消依頼済）".
     03  JOUTAI4    PIC    N(08) VALUE NC"（取消済）".
 LINKAGE              SECTION.
* パラメーター（B) エリア
*01  PARA-AREA.
 01  PARA-TEIKEI                  PIC  X(01).
 01  PARA-SHURUI                  PIC  X(01).
 01  PARA-JYUSINHI                PIC  9(08).
 01  PARA-JYUSINTIME              PIC  9(04).
 01  PARA-TOKCD                   PIC  9(08).
 01  PARA-SOUJYUSIN               PIC  9(08).
*------------------------------------------------------------*
*
**************************************************************
 PROCEDURE             DIVISION     USING  PARA-TEIKEI
                                           PARA-SHURUI
                                           PARA-JYUSINHI
                                           PARA-JYUSINTIME
                                           PARA-TOKCD
                                           PARA-SOUJYUSIN.
**************************************************************
 DECLARATIVES.
 FILEERR-SEC1              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE BMSHACF.
     MOVE        HAC-ST     TO       E-ST.
     MOVE        "BMSHACL1"  TO      E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     HAC-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC  THRU  MAIN-EXIT
               UNTIL  END-FLG = "1".
     PERFORM   END-SEC.
     STOP  RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               0.0       *
****************************************************************
 INIT-SEC              SECTION.
*
     DISPLAY  "***  START SBM9010B  ***"   UPON  CONS.
     MOVE     "INIT-SEC"     TO   S-NAME.
*ファイルのＯＰＥＮ
     OPEN      I-O    BMSHACF.
*
     INITIALIZE              HAC-REC.
*
     IF  PARA-TEIKEI  =  "2"
         GO  TO   INSTART-200
     END-IF.
* 定型
 INSTART-100.
     MOVE     ZERO                TO   HAC-F011.
     GO  TO    INSTART-300.
* 非定型
 INSTART-200.
     IF  PARA-SHURUI  =  "1"
*
         MOVE     PARA-JYUSINHI       TO   HAC-F011
         MOVE     PARA-JYUSINTIME     TO   HAC-F012
         MOVE     PARA-TOKCD          TO   HAC-F013
         MOVE     PARA-JYUSINHI       TO   WK-BR-KEY21
         MOVE     PARA-JYUSINTIME     TO   WK-BR-KEY22
         MOVE     PARA-TOKCD          TO   WK-BR-KEY23
     ELSE
         MOVE     PARA-SOUJYUSIN      TO   HAC-F011
     END-IF.
*
*
 INSTART-300.
     START  BMSHACF  KEY  IS  >=
                                   HAC-F011  HAC-F012
                                   HAC-F013  HAC-F02
                                   HAC-F308  HAC-F346
                                   HAC-F302  HAC-F402
*
            INVALID
            MOVE    "1"           TO   END-FLG
     END-START.
*
     IF  END-FLG  =  SPACE
         PERFORM   INREAD-SEC
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
** 定型・非定型
     IF  PARA-TEIKEI  =  "1"
         GO  TO    MAIN-100
     END-IF.
     IF  PARA-TEIKEI  =  "2"
         GO  TO    MAIN-200
     END-IF.
*
     GO  TO   MAIN-900.
**
 MAIN-100.
     IF  HAC-F011   <=   PARA-SOUJYUSIN
         PERFORM  SAKUJYO-SEC
         GO  TO   MAIN-900
     END-IF.
     IF  HAC-F011   >    PARA-SOUJYUSIN
         MOVE  "1"     TO   END-FLG
         GO  TO   MAIN-EXIT
     END-IF.
     GO  TO   MAIN-900.
**
  MAIN-200.
* 指定種別
     IF  PARA-SHURUI  =  "1"
         GO  TO  MAIN-210
     END-IF.
     IF  PARA-SHURUI  =  "2"
         GO  TO  MAIN-220
     END-IF.
**
     GO  TO   MAIN-900.
 MAIN-210.
     IF  HAC-BR-KEY2  =  WK-BR-KEY2
         PERFORM SAKUJYO-SEC
         GO  TO  MAIN-900
     END-IF.
     IF  HAC-BR-KEY2  NOT =  WK-BR-KEY2
         MOVE  "1"    TO   END-FLG
     GO  TO   MAIN-EXIT
     END-IF.
 MAIN-220.
     IF  HAC-F011     =  PARA-SOUJYUSIN
         PERFORM SAKUJYO-SEC
         GO  TO    MAIN-900
     END-IF.
     IF  HAC-F011   NOT =  PARA-SOUJYUSIN
         MOVE  "1"     TO   END-FLG
         GO  TO    MAIN-EXIT
     END-IF.
*
 MAIN-900.
      PERFORM    INREAD-SEC.
*
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*------削除件数の表示
**** IF     DLT-CNT  NOT =  ZERO
         MOVE         DLT-CNT     TO    SAKUJO-RUISEKI.
         DISPLAY   MSG-KN6A1  MSG-KN6A2  MSG-KN6A3  UPON CONS.
         DISPLAY   MSG-KN61   SAKUJO-RUISEKI  MSG-KN63
                   UPON  CONS.
         DISPLAY   MSG-KN6A1  MSG-KN6A2  MSG-KN6A3  UPON CONS.
*
     MOVE     "END-SEC"      TO   S-NAME.
*ファイルのCLOSE処理
     CLOSE     BMSHACF.
     DISPLAY  "***  END   SBM9010B  ***"   UPON  CONS.
*
 END-EXIT.
     EXIT.
*
**************************************************************
*    入力ファイル読込み
**************************************************************
 INREAD-SEC            SECTION.
     READ     BMSHACF  NEXT
         AT  END
         MOVE  "1"        TO   END-FLG
         GO  TO        SAKUJYO-EXIT
     END-READ.
*
     IF   PARA-TEIKEI  =  "2"
         MOVE     HAC-F011    TO   HAC-BR-KEY21
         MOVE     HAC-F012    TO   HAC-BR-KEY22
         MOVE     HAC-F013    TO   HAC-BR-KEY23
     END-IF.
*
     ADD      1           TO   READ-CNT.
*
 INREAD-EXIT.
     EXIT.
*
**************************************************************
*            レコード削除
**************************************************************
 SAKUJYO-SEC            SECTION.
     DELETE   BMSHACF.
     ADD      1  TO  DLT-CNT.
*
 SAKUJYO-EXIT.
        EXIT.

```

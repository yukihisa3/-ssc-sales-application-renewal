****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　流通ＢＭＳ                        *
*    業務名　　　　　　　：　                                  *
*    モジュール名　　　　：　出荷ＭＳＧレコード削除４
*    作成日／作成者　　　：　2019/06/27 NAV TAKAHASHI          *
*    更新日／更新者　　　：　                                  *
*    処理概要　　　　　　：　指定条件にしたがい、出荷ＭＳＧ    *
*    処理概要　　　　　　：　レコードを削除する　　　　　　    *
*                            納品日未指定時対応　　　　        *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SBM9063B.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2019/06/27.
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
*出荷メッセージファイル
     SELECT  CZSYKTC5  ASSIGN TO   DA-01-VI-CZSYKTC5
             ORGANIZAITION         INDEXED
             ACCESS      MODE      SEQUENTIAL
             RECORD      KEY       SYK-F011  SYK-F012
                                   SYK-F013  SYK-F349
                                   SYK-F02   SYK-F348
                                   SYK-F305  SYK-F309
                                   SYK-F302  SYK-F402
                         FILE      STATUS    SYK-ST.
****************************************************************
 DATA                DIVISION.
*************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 出荷メッセージファイル　　　　　　                 *
****************************************************************
 FD  CZSYKTC5
                       LABEL     RECORD    IS   STANDARD.
                       COPY      CZSYKTC5  OF   XFDLIB
                       JOINING   SYK       AS   PREFIX.
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
     03  SYK-ST                PIC  X(02).
*
 01  SYK-BR-KEY2.
     03  SYK-BR-KEY21         PIC  X(08).
     03  SYK-BR-KEY22         PIC  X(04).
     03  SYK-BR-KEY23         PIC  X(08).
     03  SYK-BR-KEY24         PIC  X(02).
     03  SYK-BR-KEY25         PIC  X(08).
 01  WK-BR-KEY2.
     03  WK-BR-KEY21          PIC  X(08).
     03  WK-BR-KEY22          PIC  X(04).
     03  WK-BR-KEY23          PIC  X(08).
     03  WK-BR-KEY24          PIC  X(02).
     03  WK-BR-KEY25          PIC  X(08).
*
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  SYK-ERR           PIC N(20) VALUE
         NC"出荷メッセージ累積ファイルエラー".
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
 LINKAGE              SECTION.
* パラメーター（B) エリア
 01  PARA-JYUSINHI                PIC  9(08).
 01  PARA-JYUSINTIME              PIC  9(04).
 01  PARA-TOKCD                   PIC  9(08).
 01  PARA-BASYO                   PIC  X(02).
 01  PARA-JYOGAI-BASYO            PIC  X(02).
 01  PARA-NOUHINBI                PIC  9(08).
*------------------------------------------------------------*
*
**************************************************************
 PROCEDURE             DIVISION     USING  PARA-JYUSINHI
                                           PARA-JYUSINTIME
                                           PARA-TOKCD
                                           PARA-BASYO
                                           PARA-JYOGAI-BASYO
                                           PARA-NOUHINBI.
**************************************************************
 DECLARATIVES.
 FILEERR-SEC1              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE CZSYKTC5.
     MOVE        SYK-ST     TO       E-ST.
     MOVE        "CZSYKTC5"  TO      E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SYK-ERR   UPON      CONS.
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
     DISPLAY  "***  START SBM9063B  ***"   UPON  CONS.
     MOVE     "INIT-SEC"     TO   S-NAME.
*ファイルのＯＰＥＮ
     OPEN      I-O    CZSYKTC5.
*
     INITIALIZE              SYK-REC.
*
 INSTART-200.
     MOVE     SPACE               TO   SYK-REC.
     INITIALIZE                        SYK-REC.
     MOVE     PARA-JYUSINHI       TO   SYK-F011.
     MOVE     PARA-JYUSINTIME     TO   SYK-F012.
     MOVE     PARA-TOKCD          TO   SYK-F013.
     MOVE     PARA-BASYO          TO   SYK-F02.
     MOVE     PARA-NOUHINBI       TO   SYK-F349.
*    IF       PARA-BASYO    =    "TK"
*             MOVE     ZERO       TO   SYK-F349
*    END-IF.
*
 INSTART-300.
     START  CZSYKTC5  KEY  IS  >=
                                   SYK-F011  SYK-F012
                                   SYK-F013  SYK-F349
                                   SYK-F02   SYK-F348
                                   SYK-F305  SYK-F309
                                   SYK-F302  SYK-F402
*
            INVALID
            MOVE    "1"           TO   END-FLG
     END-START.
*
     IF  END-FLG  =  SPACE
        PERFORM    INREAD-SEC
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*
     DELETE  CZSYKTC5.
     ADD     1        TO    DLT-CNT.
 MAIN-900.
      PERFORM    INREAD-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*------削除件数の表示
     DISPLAY "READ-CNT   = " READ-CNT      UPON CONS.
     DISPLAY "DELETE-CNT = " DLT-CNT       UPON CONS.
*
     MOVE     "END-SEC"      TO   S-NAME.
*ファイルのCLOSE処理
     CLOSE     CZSYKTC5.
     DISPLAY  "***  END   SBM9063B  ***"   UPON  CONS.
*
 END-EXIT.
     EXIT.
*
**************************************************************
*    入力ファイル読込み
**************************************************************
 INREAD-SEC            SECTION.
     READ     CZSYKTC5  NEXT
         AT  END
         MOVE  "1"        TO   END-FLG
         GO  TO        INREAD-EXIT
     END-READ.
*バッチ_チェック
     IF  SYK-F011  =  PARA-JYUSINHI
     AND SYK-F012  =  PARA-JYUSINTIME
     AND SYK-F013  =  PARA-TOKCD
         CONTINUE
     ELSE
         MOVE  "1"        TO   END-FLG
         GO  TO        INREAD-EXIT
     END-IF.
*場所ＣＤチェック
     IF  PARA-BASYO NOT =  SPACE
         IF  SYK-F02  NOT =  PARA-BASYO
             GO           TO   INREAD-SEC
         END-IF
     END-IF.
*削除除外出荷場所判定
*倉庫指定がない場合で、除外倉庫の場合は削除対象としない。
     IF  PARA-JYOGAI-BASYO  NOT =  SPACE
         IF  PARA-BASYO         =  SPACE
         AND PARA-JYOGAI-BASYO  =  SYK-F02
             GO           TO   INREAD-SEC
         END-IF
     END-IF.
*納品日チェック
     IF  PARA-NOUHINBI  NOT =  ZERO
*        IF   PARA-BASYO    = "TK"
*             IF  SYK-F348   NOT =  PARA-NOUHINBI
*                 GO           TO   INREAD-SEC
*             END-IF
*        ELSE
              IF  SYK-F349   NOT =  PARA-NOUHINBI
                  GO           TO   INREAD-SEC
              END-IF
*        END-IF
     END-IF.
*
     ADD      1           TO   READ-CNT.
*
 INREAD-EXIT.
     EXIT.

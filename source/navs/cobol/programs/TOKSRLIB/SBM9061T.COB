****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　流通ＢＭＳ                        *
*    業務名　　　　　　　：　                                  *
*    モジュール名　　　　：　出荷ＭＳＧレコード削除
*    作成日／作成者　　　：　2018/03/19 INOUE                  *
*    更新日／更新者　　　：　                                  *
*    処理概要　　　　　　：　指定条件にしたがい、出荷ＭＳＧ    *
*    　　　　　　　　　　：　レコードを削除する　　　　　　    *
*    更新日／更新者　　　：　2020/08/18 NAV TAKAHASHI          *
*    処理概要　　　　　　：　出荷送信済の場合は削除します。    *
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SBM9061T.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2018/03/19.
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
     SELECT  BMSSYKF  ASSIGN TO    DA-01-VI-BMSSYKL3
             ORGANIZAITION         INDEXED
             ACCESS      MODE      SEQUENTIAL
             RECORD      KEY       SYK-F011  SYK-F012
                                   SYK-F013  SYK-F02
                                   SYK-F349  SYK-F309
                                   SYK-F302  SYK-F402
                         FILE      STATUS    SYK-ST.
****************************************************************
 DATA                DIVISION.
*************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 出荷メッセージファイル　　　　　　                 *
****************************************************************
 FD  BMSSYKF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      BMSSYKF   OF   XFDLIB
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
         NC"「＊出荷メッセージ　　削除件数　＝　".
         05  SAKUJO-RUISEKI  PIC 9(08).
         05  MSG-KN63  PIC    N(05) VALUE NC"　件　＊」".
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
 01  PARA-BASYO                   PIC  X(02).
 01  PARA-NOUHINBI                PIC  9(08).
*------------------------------------------------------------*
*
**************************************************************
 PROCEDURE             DIVISION     USING  PARA-TEIKEI
                                           PARA-SHURUI
                                           PARA-JYUSINHI
                                           PARA-JYUSINTIME
                                           PARA-TOKCD
                                           PARA-BASYO
                                           PARA-NOUHINBI.
**************************************************************
 DECLARATIVES.
 FILEERR-SEC1              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE BMSSYKF.
     MOVE        SYK-ST     TO       E-ST.
     MOVE        "BMSSYKL3"  TO      E-FILE.
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
     DISPLAY  "***  START SBM9061T  ***"   UPON  CONS.
     MOVE     "INIT-SEC"     TO   S-NAME.
*ファイルのＯＰＥＮ
     OPEN      I-O    BMSSYKF.
*
     INITIALIZE              SYK-REC.
*
     IF  PARA-TEIKEI  =  "2"
         GO  TO   INSTART-200
     END-IF.
* 定型
 INSTART-100.
     GO  TO    INIT-EXIT.
*    MOVE       ZERO              TO   SYK-F011.
*    GO  TO    INSTART-300.
* 非定型
 INSTART-200.
     IF  PARA-SHURUI  =  "1"
         MOVE     PARA-JYUSINHI       TO   SYK-F011
         MOVE     PARA-JYUSINTIME     TO   SYK-F012
         MOVE     PARA-TOKCD          TO   SYK-F013
         MOVE     PARA-BASYO          TO   SYK-F02
         MOVE     PARA-NOUHINBI       TO   SYK-F349
         MOVE     SPACE               TO   SYK-F309
         MOVE     SPACE               TO   SYK-F302
         MOVE     SPACE               TO   SYK-F402
         MOVE     PARA-JYUSINHI       TO   WK-BR-KEY21
         MOVE     PARA-JYUSINTIME     TO   WK-BR-KEY22
         MOVE     PARA-TOKCD          TO   WK-BR-KEY23
         MOVE     PARA-BASYO          TO   WK-BR-KEY24
         MOVE     PARA-NOUHINBI       TO   WK-BR-KEY25
     ELSE
         MOVE     "1"                 TO   END-FLG
         GO                           TO   INIT-EXIT
     END-IF.
*
*
 INSTART-300.
     START  BMSSYKF  KEY  IS  >=
                                   SYK-F011  SYK-F012
                                   SYK-F013  SYK-F02
                                   SYK-F349  SYK-F309
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
** 定型・非定型
*    IF  PARA-TEIKEI  =  "1"
*        GO  TO    MAIN-100
*    END-IF.
     IF  PARA-TEIKEI  =  "2"
         GO  TO    MAIN-200
     END-IF.
*
     GO  TO   MAIN-900.
**
*MAIN-100.
*    IF  SYK-F011   <=   PARA-BASYO
*        PERFORM  SAKUJYO-SEC
*        GO  TO   MAIN-900
*    END-IF.
*    IF  SYK-F011   >    PARA-BASYO
*        MOVE  "1"     TO   END-FLG
*        GO  TO   MAIN-EXIT
*    END-IF.
*    GO  TO   MAIN-900.
**
  MAIN-200.
* 指定種別
     IF  PARA-SHURUI  =  "1"
         GO  TO  MAIN-210
     END-IF.
*    IF  PARA-SHURUI  =  "2"
*        GO  TO  MAIN-220
*    END-IF.
**
     GO  TO   MAIN-900.
 MAIN-210.
     IF  SYK-BR-KEY2  =  WK-BR-KEY2
*********2020/08/18 NAV ST 削除条件追加
*********PERFORM SAKUJYO-SEC
***      IF  SYK-F601  =  SPACE  *>未送信時のみ削除対象とする。
             PERFORM SAKUJYO-SEC
***      END-IF
*********2020/08/18 NAV ED 削除条件追加
         GO  TO  MAIN-900
     END-IF.
     IF  SYK-BR-KEY2  NOT =  WK-BR-KEY2
         MOVE  "1"    TO   END-FLG
     GO  TO   MAIN-EXIT
     END-IF.
*MAIN-220.
*    IF  SYK-F011     =  PARA-BASYO
*        PERFORM SAKUJYO-SEC
*        GO  TO    MAIN-900
*    END-IF.
*    IF  SYK-F011   NOT =  PARA-BASYO
*        MOVE  "1"     TO   END-FLG
*        GO  TO    MAIN-EXIT
*    END-IF.
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
     CLOSE     BMSSYKF.
     DISPLAY  "***  END   SBM9061T  ***"   UPON  CONS.
*
 END-EXIT.
     EXIT.
*
**************************************************************
*    入力ファイル読込み
**************************************************************
 INREAD-SEC            SECTION.
     READ     BMSSYKF  NEXT
         AT  END
         MOVE  "1"        TO   END-FLG
         GO  TO        INREAD-EXIT
     END-READ.
*
     IF   PARA-TEIKEI  =  "2"
         MOVE     SYK-F011    TO   SYK-BR-KEY21
         MOVE     SYK-F012    TO   SYK-BR-KEY22
         MOVE     SYK-F013    TO   SYK-BR-KEY23
         MOVE     SYK-F02     TO   SYK-BR-KEY24
         MOVE     SYK-F349    TO   SYK-BR-KEY25
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
     DELETE   BMSSYKF.
     ADD      1  TO  DLT-CNT.
*
 SAKUJYO-EXIT.
        EXIT.

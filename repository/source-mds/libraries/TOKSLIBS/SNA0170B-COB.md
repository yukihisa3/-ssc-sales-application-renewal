# SNA0170B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SNA0170B.COB`

## ソースコード

```cobol
***********************************************************
*
*    顧客名　　　　　：（株）サカタのタネ殿
*    サブシステム　　：ＨＧ基幹システム苗業務連携
*    業務名　　　　　：
*    モジュール名　　：連携漏れデータチェック
*    作成日／更新日　：2011/10/31
*    作成者／更新者　：飯田/NAV
*    処理概要　　　　：
*      苗業務システムへの連携漏れの有無をチェックするため、
*      売上伝票ファイルの状態を判定し、未連携の明細をリスト
*      出力する。そのためのデータ抽出。
*    作成日／更新日　：2012/07/18
*    作成者／更新者　：井上/NAV
*    処理概要　　　　：
*      売上伝票データの小売連携区分はノーチェックにし、
*      倉庫マスタの小売連携区分をチェックする。
*
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNA0170B.
 AUTHOR.               S.I.
 DATE-WRITTEN.         2011/10/31.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*売上伝票ファイル
     SELECT  SHTDENF
       ASSIGN      TO  SHTDENLE
       ORGANIZATION    INDEXED
       ACCESS    MODE  SEQUENTIAL
       RECORD    KEY
*↓2012/07/18
****     URI-F32  *> 小売連携区分
*↑2012/07/18
         URI-F33  *> 連携Ｎｏ
         URI-F112 *> 納品日
       FILE      STATUS    URI-ST.
*
*チェックリスト中間ファイル
     SELECT  NARERRF
       ASSIGN      TO  NARERRL1
       ORGANIZATION    INDEXED
       ACCESS    MODE  RANDOM
       RECORD    KEY
         ERR-F01 *> 担当者コード
         ERR-F03 *> 受注種別
         ERR-F04 *> 取引先コード
         ERR-F06 *> 納品日
         ERR-F07 *> 自社商品コード
         ERR-F08 *> 品単コード
       FILE      STATUS    ERR-ST.
*
*取引先マスタ
     SELECT  HTOKMS
       ASSIGN      TO  TOKMS2
       ORGANIZATION    INDEXED
       ACCESS    MODE  RANDOM
       RECORD    KEY
         TOK-F01 *> 相手取引先ＣＤ
       FILE STATUS     TOK-ST.
*
*商品名称マスタ
     SELECT  HMEIMS
       ASSIGN    TO    MEIMS1
       ORGANIZATION    INDEXED
       ACCESS    MODE  RANDOM
       RECORD    KEY
         MEI-F011  *> 商品コード
         MEI-F0121 *> 単１
         MEI-F0122 *> 単２
         MEI-F0123 *> 単３
       FILE STATUS     MEI-ST.
*
*↓2012/07/18
*倉庫マスタ
     SELECT  ZSOKMS     ASSIGN    TO        ZSOKMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SOK-F01
                        FILE      STATUS    IS   SOK-ST.
*↑2012/07/18
*
*担当者マスタ
*    SELECT  HTANMS
*      ASSIGN    TO    TANMS1
*      ORGANIZATION    INDEXED
*      ACCESS    MODE  RANDOM
*      RECORD    KEY
*        TAN-F01 *> 部門ＣＤ
*        TAN-F02 *> 担当者ＣＤ
*      FILE STATUS     TAN-ST.
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 売上伝票ファイル                                   *
****************************************************************
 FD  SHTDENF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      SHTDENF   OF   XFDLIB
                       JOINING   URI       AS   PREFIX.
****************************************************************
*    FILE = チェックリスト中間ファイル                         *
****************************************************************
 FD  NARERRF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NARERRF   OF   XFDLIB
                       JOINING   ERR       AS   PREFIX.
****************************************************************
*    FILE = 取引先マスタ                               *
****************************************************************
 FD  HTOKMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS   OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*    FILE = 商品名称マスタ                               *
****************************************************************
 FD  HMEIMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HMEIMS   OF   XFDLIB
                       JOINING   MEI       AS   PREFIX.
*↓2012/07/18
******************************************************************
*    倉庫マスタ
******************************************************************
 FD  ZSOKMS             LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
*↑2012/07/18
****************************************************************
*    FILE = 担当者マスタ                               *
****************************************************************
*FD  HTANMS
*                      LABEL     RECORD    IS   STANDARD.
*                      COPY      HTANMS    OF   XFDLIB
*                      JOINING   TAN       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  URI-ST             PIC  X(02).
     03  ERR-ST             PIC  X(02).
     03  TOK-ST             PIC  X(02).
     03  MEI-ST             PIC  X(02).
*↓2012/07/18
     03  SOK-ST             PIC  X(02).
*↑2012/07/18
*    03  TAN-ST             PIC  X(02).
*フラグ領域
 01  FLG-AREA.
     03  END-FLG            PIC  X(03)  VALUE  SPACE.
     03  FG-SHTDENF-END     PIC  X(03)  VALUE  SPACE.
     03  FG-NARERRF-INV     PIC  9(01)  VALUE  ZERO.
     03  FG-HMEIMS-INV      PIC  9(01)  VALUE  ZERO.
     03  FG-HTOKMS-INV      PIC  9(01)  VALUE  ZERO.
     03  FG-HTANMS-INV      PIC  9(01)  VALUE  ZERO.
*↓2012/07/18
     03  FG-ZSOKMS-INV      PIC  9(01)  VALUE  ZERO.
*↑2012/07/18
*
*ワーク領域
 01  WRK-AREA.
     03  CT-IN              PIC  9(08).
     03  CT-OT              PIC  9(08).
     03  WK-DMY-ACCEPT      PIC  X(01).
     03  WK-YMD             PIC  9(08).
     03  WK-YMDR  REDEFINES WK-YMD.
       05  WK-YMD-Y         PIC  9(04).
       05  WK-YMD-M         PIC  9(02).
       05  WK-YMD-D         PIC  9(02).
*
     03  WK-CMPYMD.
       05  WK-CMPYMD-Y      PIC S9(04).
       05  WK-CMPYMD-M      PIC S9(02).
       05  WK-CMPYMD-D      PIC S9(02).
*
     03  WK-SYO             PIC  9(06).
     03  WK-AMARI           PIC  9(06).
     03  WK-MATUBI          PIC  9(02).
*
     03  IX                 PIC  9(04).
*
     03  WK-REN-NO-X        PIC  X(09).
     03  WK-REN-NO REDEFINES WK-REN-NO-X
                            PIC  9(09).
*
     03  WK-KJN-NOHNBI-9    PIC  9(08).
     03  WK-KJN-NOHNBI  REDEFINES WK-KJN-NOHNBI-9.
       05  WK-KJN-NOHNBI-Y  PIC  9(04).
       05  WK-KJN-NOHNBI-M  PIC  9(02).
       05  WK-KJN-NOHNBI-D  PIC  9(02).
*
     03  INF-KEY.
       05  INF-F112         PIC  9(08). *> 納品日
*
     03  BRK-KEY.
       05  BRK-F112         PIC  9(08). *> 納品日
*
     03  WK-MEISAISU        PIC  9(06).
     03  WK-IRAI-YOTEISU    PIC  9(06).
*
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER             PIC  X(05)     VALUE " *** ".
     03  S-NAME             PIC  X(30).
*
*システム日付／時刻
 01  TIME-AREA.
     03  WK-TIME            PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-DATE            PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE           PIC  9(08)  VALUE  ZERO.
*
*受信時間チェック
 01  WK-JIMEI.
     03  WK-HH              PIC   9(02)  VALUE  ZERO.
     03  WK-MM              PIC   9(02)  VALUE  ZERO.
*
*日付論理チェック
 01  WK-CHKDATE.
     03  WK-CHKDATE-YYYY    PIC   9(04)  VALUE  ZERO.
     03  WK-CHKDATE-MM      PIC   9(02)  VALUE  ZERO.
     03  WK-CHKDATE-DD      PIC   9(02)  VALUE  ZERO.
*
 01  MSG-WAKU               PIC  N(21)  VALUE
     NC"＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
 01  MSG01                  PIC  N(21)  VALUE
     NC"＊　連携漏れの売上伝票はありません　　　＊".
 01  MSG02.
     03  MSG021             PIC  N(21)  VALUE
     NC"＊　連携漏れの売上伝票の抽出終了　　　　＊".
     03  MSG022.
       05  FILLER           PIC  X(20)  VALUE
       "＊　　　件　数　＝".
       05  FILLER           PIC  X(01)  VALUE SPACE.
       05  MSG022-KENSU     PIC  ZZZZZZ VALUE ZERO.
       05  FILLER           PIC  X(01)  VALUE SPACE.
       05  FILLER           PIC  X(18)  VALUE
       "件　　　　　　＊".
*
 01  FILE-ERR.
     03  URI-ERR           PIC  N(20)  VALUE
         NC"売上伝票ファイルエラー".
     03  ERR-ERR           PIC  N(20)  VALUE
         NC"チェックリスト中間ワークエラー".
     03  TOK-ERR           PIC  N(20)  VALUE
         NC"取引先マスタエラー".
     03  MEI-ERR           PIC  N(20)  VALUE
         NC"商品名称マスタエラー".
*↓2012/07/18
     03  SOK-ERR           PIC  N(20)  VALUE
         NC"倉庫マスタエラー".
*↑2012/07/18
*    03  TAN-ERR           PIC  N(20)  VALUE
*        NC"担当者マスタエラー".
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
****************************************************************
 LINKAGE               SECTION.
****************************************************************
* 入力パラメータ
*
* 出力パラメータ
*
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 URI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SHTDENF.
     DISPLAY     URI-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     URI-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 ERR-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NARERRF.
     DISPLAY     ERR-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     DISPLAY     TOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HMEIMS.
     DISPLAY     MEI-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     MEI-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*↓2012/07/18
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     DISPLAY     SOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*↑2012/07/18
*TAN-ERR                   SECTION.
*    USE         AFTER     EXCEPTION PROCEDURE HTANMS.
*    DISPLAY     TAN-ERR   UPON      CONS.
*    DISPLAY     SEC-NAME  UPON      CONS.
*    DISPLAY     TAN-ST    UPON      CONS.
*    MOVE        "4000"    TO        PROGRAM-STATUS.
*    STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE  "PROCESS START"       TO  S-NAME.

     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC  UNTIL END-FLG = "END".
     PERFORM  END-SEC.

     STOP RUN.
 CONTROL-EXIT.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE  "INIT-SEC"       TO  S-NAME.

     DISPLAY   "**  START SNA0170B  **"  UPON CONS.

     PERFORM  SDATE-GET-SEC.

     MOVE  "INIT-SEC"       TO  S-NAME.
*ファイルのＯＰＥＮ
     OPEN  INPUT SHTDENF.
     OPEN  INPUT HTOKMS.
*↓2012/07/18
     OPEN  INPUT ZSOKMS.
*↑2012/07/18
** 2011/11/17,S  S.I/NAV
**     OPEN  INPUT HMEIMS.
**     OPEN  INPUT HTANMS.
** 2011/11/17,E  S.I/NAV
     OPEN  OUTPUT NARERRF.
     CLOSE NARERRF.
     OPEN  I-O    NARERRF.
*ワークの初期化
     INITIALIZE  WRK-AREA.
     INITIALIZE  FLG-AREA.

     PERFORM  KIJYUN-NOHNBI-GET-SEC. *> 基準納品日の取得

     INITIALIZE  URI-REC.
*↓2012/07/18
**** MOVE  "1"              TO  URI-F32.  *> 小売連携区分
*↑2012/07/18
     MOVE  LOW-VALUE        TO  FG-SHTDENF-END.

     PERFORM  RD-SHTDENF-SEC.  *> 売上伝票の初期読込み
     IF  FG-SHTDENF-END = "END"
         DISPLAY  MSG-WAKU  UPON CONS
         DISPLAY  MSG01     UPON CONS
         DISPLAY  MSG-WAKU  UPON CONS
         MOVE  "END"        TO  END-FLG
         GO TO  INIT-EXIT
     END-IF.


 INIT-EXIT.
     EXIT.
****************************************************************
*             システム日付取得
****************************************************************
 SDATE-GET-SEC              SECTION.
     MOVE  "SDATE-GET-SEC"  TO  S-NAME.
*システム日付・時刻の取得
     ACCEPT  WK-DATE   FROM DATE.
     MOVE  "3"              TO  LINK-IN-KBN.
     MOVE  WK-DATE          TO  LINK-IN-YMD6.
     MOVE  ZERO             TO  LINK-IN-YMD8.
     MOVE  ZERO             TO  LINK-OUT-RET.
     MOVE  ZERO             TO  LINK-OUT-YMD.
     CALL  "SKYDTCKB"  USING LINK-IN-KBN
                             LINK-IN-YMD6
                             LINK-IN-YMD8
                             LINK-OUT-RET
                             LINK-OUT-YMD.
     MOVE  LINK-OUT-YMD     TO  SYS-DATE.
     ACCEPT  WK-TIME   FROM TIME.
 SDATE-GET-EXIT.
     EXIT.
****************************************************************
*    基準納品日取得処理
****************************************************************
 KIJYUN-NOHNBI-GET-SEC      SECTION.
     MOVE  "KIJYUN-HOHNBI-GET-SEC"  TO  S-NAME.

     DISPLAY  NC"基準納品日を入力して下さい。"
                " ""00000000"""
              NC"入力で処理を中止します。"  UPON CONS.
     ACCEPT  WK-KJN-NOHNBI  FROM CONS.

     IF  WK-KJN-NOHNBI = ZERO
         MOVE  4010        TO  PROGRAM-STATUS
         EXIT PROGRAM
     END-IF.

* 基準納品日の基本チェック
     MOVE  "2"             TO  LINK-IN-KBN.
     MOVE  WK-KJN-NOHNBI   TO  LINK-IN-YMD8.
     CALL  "SKYDTCKB" USING LINK-IN-KBN
                            LINK-IN-YMD6
                            LINK-IN-YMD8
                            LINK-OUT-RET
                            LINK-OUT-YMD.
     IF  LINK-OUT-RET NOT = ZERO
         DISPLAY
           NC"入力内容に誤りがあります、再度入力して下さい。"
           NC"　　　　　　　　　"
           NC"ＥＮＴＥＲを押下して下さい。"  UPON CONS
         ACCEPT  WK-DMY-ACCEPT  FROM CONS
         GO TO  KIJYUN-NOHNBI-GET-SEC
     END-IF.

 KIJYUN-NOHNBI-GET-EXIT.
     EXIT.
****************************************************************
*    売上伝票ファイル読込み　                                  *
****************************************************************
 RD-SHTDENF-SEC          SECTION.
     MOVE  "RD-SHTDENF-SEC" TO  S-NAME.

     IF  FG-SHTDENF-END = LOW-VALUE
         START  SHTDENF  KEY >=
*↓2012/07/18
****            URI-F32  *> 小売連携区分
*↑2012/07/18
                URI-F33  *> 連携Ｎｏ
                URI-F112 *> 納品日
           INVALID KEY
              MOVE  "END"   TO  FG-SHTDENF-END
              GO TO  RD-SHTDENF-EXIT
         END-START

         MOVE  SPACE        TO  FG-SHTDENF-END

     END-IF.

     READ  SHTDENF
       AT  END
         MOVE  "END"        TO  FG-SHTDENF-END
         GO TO  RD-SHTDENF-EXIT
     END-READ.

*↓2012/07/18
**** IF  URI-F32 = "1" *> 小売連携区分あり
****     CONTINUE
**** ELSE
****     MOVE  "END"        TO  FG-SHTDENF-END
****     GO TO  RD-SHTDENF-EXIT
**** END-IF.
*↑2012/07/18

     IF  URI-F33 = SPACE  *> 連携Ｎｏ
         CONTINUE
     ELSE
         MOVE  "END"        TO  FG-SHTDENF-END
         GO TO  RD-SHTDENF-EXIT
     END-IF.

     IF  URI-F112 <= WK-KJN-NOHNBI-9 *> 納品日
         CONTINUE
     ELSE
         MOVE  "END"        TO  FG-SHTDENF-END
         GO TO  RD-SHTDENF-EXIT
     END-IF.
*
*↓2012/07/18
     MOVE    URI-F48        TO  SOK-F01.
     PERFORM RD-ZSOKMS-SEC.
     IF    FG-ZSOKMS-INV  = 1
           GO TO  RD-SHTDENF-SEC
     END-IF.
     IF    SOK-F12   NOT = "1"
           GO TO  RD-SHTDENF-SEC
     END-IF.
*↑2012/07/18
*
     ADD 1   TO  CT-IN.
*
 RD-SHTDENF-EXIT.
     EXIT.
*
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE  "MAIN-SEC"       TO  S-NAME.
*
     PERFORM  UNTIL FG-SHTDENF-END = "END"

       PERFORM  OTDT-EDWT-SEC

       PERFORM  RD-SHTDENF-SEC
       IF  FG-SHTDENF-END = "END"
           MOVE  "END"      TO  END-FLG
       END-IF

     END-PERFORM.

* 終了メッセージ
     DISPLAY  MSG-WAKU  UPON CONS.
     DISPLAY  MSG021    UPON CONS.
     MOVE  CT-OT            TO  MSG022-KENSU.
     DISPLAY  MSG022    UPON CONS.
     DISPLAY  MSG-WAKU  UPON CONS.

 MAIN-EXIT.
     EXIT.
****************************************************************
*  編集／出力処理                                              *
****************************************************************
 OTDT-EDWT-SEC              SECTION.
     MOVE  "OTDT-EDWT-SEC"  TO  S-NAME.

* 取引先マスタ
     MOVE  URI-F01          TO  TOK-F01. *> 取引先ＣＤ

     READ  HTOKMS
       INVALID
         MOVE  1            TO  FG-HTOKMS-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-HTOKMS-INV
     END-READ.

** 2011/11/17,S  S.I/NAV
* 商品名称マスタ
**     MOVE  URI-F1411        TO  MEI-F011. *> 商品ＣＤ
**     MOVE  URI-F1412        TO  MEI-F012. *> 品単
**
**     READ  HMEIMS
**       INVALID
**         MOVE  1            TO  FG-HMEIMS-INV
**       NOT INVALID
**         MOVE  ZERO         TO  FG-HMEIMS-INV
**     END-READ.
** 2011/11/17,E  S.I/NAV

** 2011/11/17,S  S.I/NAV 担当者名は廃止
* 担当者マスタ
**     MOVE  "2920"           TO  TAN-F01. *> 部門ＣＤ
**     MOVE  URI-F60          TO  TAN-F02. *> 担当者ＣＤ
**
**     READ  HTANMS
**       INVALID
**         MOVE  1            TO  FG-HTANMS-INV
**       NOT INVALID
**         MOVE  ZERO         TO  FG-HTANMS-INV
**     END-READ.
** 2011/11/17,E  S.I/NAV

* キー設定
     PERFORM  OTDT-KY-SEC.

*    KEY
*      ERR-F01 担当者コード
*      ERR-F03 受注種別
*      ERR-F04 取引先コード
*      ERR-F06 納品日
*      ERR-F07 自社商品コード
*      ERR-F08 品単コード

     READ  NARERRF
       INVALID
         MOVE  1            TO  FG-NARERRF-INV
       NOT INVALID
         MOVE  ZERO         TO  FG-NARERRF-INV
     END-READ.

     IF  FG-NARERRF-INV = 1
         INITIALIZE  ERR-REC
         PERFORM  OTDT-KY-SEC
         PERFORM  OTDT-ED-SEC
         PERFORM  OTDT-ED2-SEC
         WRITE  ERR-REC
         ADD  1   TO  CT-OT
     ELSE
         PERFORM  OTDT-ED2-SEC
         REWRITE  ERR-REC
     END-IF.



 OTDT-EDWT-EXIT.
     EXIT.
****************************************************************
*  編集処理（キー）                                            *
****************************************************************
 OTDT-KY-SEC           SECTION.
     MOVE  URI-F60          TO  ERR-F01.  *> 担当者コード

     IF  URI-F274 = ZERO *> オンライン区分
         MOVE  "1"          TO  ERR-F03   *> 受注種別(手書き)
     ELSE
         MOVE  "2"          TO  ERR-F03   *> 受注種別(ｵﾝﾗｲﾝ)
     END-IF.

     MOVE  URI-F01          TO  ERR-F04.  *> 取引先コード

     MOVE  URI-F112         TO  ERR-F06.  *> 納品日
     MOVE  URI-F1411        TO  ERR-F07.  *> 自社商品コード
     MOVE  URI-F1412        TO  ERR-F08.  *> 品単コード

 OTDT-KY-EXIT.
     EXIT.
****************************************************************
*  編集処理                                                    *
****************************************************************
 OTDT-ED-SEC           SECTION.

** 2011/11/17,S  S.I/NAV 担当者名は廃止
**     IF  FG-HTANMS-INV = ZERO
**         MOVE  TAN-F03      TO  ERR-F02   *> 担当者名
**     ELSE
**         MOVE  SPACE        TO  ERR-F02   *> 担当者名
**     END-IF.

     MOVE  SPACE            TO  ERR-F02.  *> 担当者名
** 2011/11/17,E  S.I/NAV

     IF  FG-HTOKMS-INV = ZERO
         MOVE  TOK-F02      TO  ERR-F05   *> 取引先名
     ELSE
         MOVE  SPACE        TO  ERR-F05   *> 取引先名
     END-IF.

     MOVE  URI-F25          TO  ERR-F09.  *> 相手商品コード

*↓商品名称ﾏｽﾀではなく売上伝票ﾌｧｲﾙよりｾｯﾄ
*    IF  FG-HMEIMS-INV = ZERO
*        MOVE  MEI-F021     TO  ERR-F101  *> 商品名１
*        MOVE  MEI-F022     TO  ERR-F102  *> 商品名２
*    ELSE
*        MOVE  SPACE        TO  ERR-F101  *> 商品名１
*        MOVE  SPACE        TO  ERR-F102  *> 商品名２
*    END-IF.
*
     MOVE  URI-F1421        TO  ERR-F101. *> 商品名１
     MOVE  URI-F1422        TO  ERR-F102. *> 商品名２
*↑商品名称ﾏｽﾀではなく売上伝票ﾌｧｲﾙよりｾｯﾄ
*
     MOVE  SYS-DATE         TO  ERR-F98.  *> 作成日付
     MOVE  WK-TIME (1:6)    TO  ERR-F99.  *> 作成時刻
*
 OTDT-ED-EXIT.
     EXIT.
****************************************************************
*  編集２処理                                                  *
****************************************************************
 OTDT-ED2-SEC           SECTION.
     COMPUTE  ERR-F11 = ERR-F11 + 1.       *> 明細数
     COMPUTE  ERR-F12 = ERR-F12 + URI-F15. *> 数量

 OTDT-ED2-EXIT.
     EXIT.
*
*↓2012/07/18
****************************************************************
*    倉庫マスタ検索                                            *
****************************************************************
 RD-ZSOKMS-SEC          SECTION.
     READ  ZSOKMS
       INVALID
         MOVE  1                 TO  FG-ZSOKMS-INV
       NOT INVALID
         MOVE  0                 TO  FG-ZSOKMS-INV
     END-READ.
 RD-ZSOKMS-EXIT.
     EXIT.
*↑2012/07/18
*
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE  "END-SEC"        TO  S-NAME.
*ファイル ＣＬＯＳＥ
     CLOSE  SHTDENF.
     CLOSE  HTOKMS.
*↓2012/07/18
     CLOSE  ZSOKMS.
*↑2012/07/18
** 2011/11/17,S  S.I/NAV
**     CLOSE  HMEIMS.
**     CLOSE  HTANMS.
** 2011/11/17,E  S.I/NAV
     CLOSE  NARERRF.
     DISPLAY   "**  END   SNA0170B  **"  UPON CONS.

 END-EXIT.
     EXIT.
*****************<<  SNA0170B   END PROGRAM  >>******************

```

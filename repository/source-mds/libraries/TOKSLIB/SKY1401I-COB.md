# SKY1401I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SKY1401I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理サブシステム              *
*    業務名　　　　　　　：　オフライン出荷                    *
*    モジュール名　　　　：　倉庫別_番マスタ保守              *
*    作成日／更新日　　　：　1999/10/18                        *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　倉庫別_番マスタの保守を行なう。  *
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SKY1401I.
 AUTHOR.               T.TAKAHASHI.
 DATE-WRITTEN.         99/09/10.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*倉庫別_番マスタ
     SELECT  JHMTANF   ASSIGN    TO        JHMTANL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TAN-F01
                                           TAN-F02
                       FILE      STATUS    TAN-ST.
*商品名称マスタ
     SELECT  HMEIMS    ASSIGN    TO        MEIMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       MEI-F011
                                           MEI-F0121
                                           MEI-F0122
                                           MEI-F0123
                       FILE      STATUS    MEI-ST.
*倉庫マスタ
     SELECT  ZSOKMS    ASSIGN    TO        ZSOKMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       SOK-F01
                       FILE      STATUS    SOK-ST.
*画面定義ファイル
     SELECT  DSPFILE   ASSIGN    TO        GS-DSPF
                       FORMAT              DSP-FMT
                       GROUP               DSP-GRP
                       PROCESSING          DSP-PRO
                       FUNCTION            DSP-FNC
                       FILE      STATUS    DSP-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 倉庫別_番マスタ                                   *
****************************************************************
 FD  JHMTANF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JHMTANF   OF   XFDLIB
                       JOINING   TAN       AS   PREFIX.
****************************************************************
*    FILE = 商品名称マスタ                                     *
****************************************************************
 FD  HMEIMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HMEIMS    OF   XFDLIB
                       JOINING   MEI       AS   PREFIX.
****************************************************************
*    FILE = 倉庫マスタ                                         *
****************************************************************
 FD  ZSOKMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ZSOKMS    OF   XFDLIB
                       JOINING   SOK       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FKY14011  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  TAN-ST                   PIC  X(02).
     03  MEI-ST                   PIC  X(02).
     03  SOK-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*フラグ領域
 01  FLG-AREA.
     03  ERR-FLG                  PIC  9(01)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  JHMTANF-INV-FLG          PIC  X(03)  VALUE  SPACE.
     03  HMEIMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  ZSOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
***  モード退避
     03  SAV-SHORI                PIC  9(01)  VALUE  ZERO.
*
*システム日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-DATE                  PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE                 PIC  9(08)  VALUE  ZERO.
*システム日付変換用
  01  G-DATE.
     03  G-DATE-YY                PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  G-DATE-MM                PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  G-DATE-DD                PIC  9(02)  VALUE  ZERO.
*システム日付変換用
  01  G-TIME.
     03  G-TIME-HH                PIC  Z9.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  G-TIME-MM                PIC  Z9.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  G-TIME-SS                PIC  Z9.
*受信時間チェック
 01  WK-JIKAN.
     03  WK-HH                    PIC   9(02)  VALUE  ZERO.
     03  WK-MM                    PIC   9(02)  VALUE  ZERO.
*
*日付論理チェック
 01  WK-CHKDATE.
     03  WK-CHKDATE-YYYY          PIC   9(04)  VALUE  ZERO.
     03  WK-CHKDATE-MM            PIC   9(02)  VALUE  ZERO.
     03  WK-CHKDATE-DD            PIC   9(02)  VALUE  ZERO.
*
*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-MSG1.
         05  FILLER               PIC   N(15)
             VALUE NC"_取消　_終了".
     03  PF-MSG2.
         05  FILLER               PIC   N(15)
             VALUE NC"_取消　_終了　_項目戻し".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R   OCCURS   2   PIC   N(15).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(25)
             VALUE NC"無効キーです。".
     03  ERR-MSG2.
         05  FILLER              PIC   N(25)
             VALUE NC"正しい値を入力して下さい。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(25)
             VALUE NC"倉庫コードを入力して下さい。".
     03  ERR-MSG4.
         05  FILLER              PIC   N(25)
             VALUE NC"商品名称マスタ未登録です。".
     03  ERR-MSG5.
         05  FILLER              PIC   N(25)
             VALUE NC"倉庫別_番マスタに登録済みです。".
     03  ERR-MSG6.
         05  FILLER              PIC   N(25)
             VALUE NC"倉庫別_番マスタに未登録です。".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  6   PIC   N(25).
*
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  TAN-ERR           PIC N(15) VALUE
         NC"倉庫別_番マスタエラー".
     03  MEI-ERR           PIC N(15) VALUE
         NC"商品名称マスタエラー".
     03  SOK-ERR           PIC N(15) VALUE
         NC"倉庫マスタエラー".
     03  DSP-ERR           PIC N(15) VALUE
         NC"画面ファイルエラー".
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
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
**************************************************************
 LINKAGE               SECTION.
 01  LINK-SOKCD            PIC 9(02).
*
**************************************************************
 PROCEDURE             DIVISION   USING   LINK-SOKCD.
**************************************************************
 DECLARATIVES.
*
 TAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMTANF.
     MOVE        TAN-ST    TO        E-ST.
     MOVE        "JHMTANF " TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TAN-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HMEIMS.
     MOVE        MEI-ST    TO        E-ST.
     MOVE        "HMEIMS "  TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     MEI-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     MOVE        SOK-ST    TO        E-ST.
     MOVE        "ZSOKMS "  TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SOK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     MOVE        DSP-ST    TO        E-ST.
     MOVE        "DSPFILE"  TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     DSP-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS START"     TO   S-NAME.
***
     DISPLAY NC"倉庫" "CD = " LINK-SOKCD  UPON CONS.
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC          UNTIL   END-FLG   =   "END".
     PERFORM   END-SEC.
***
     STOP    RUN.
 CONTROL-EXIT.
     EXIT.
****************************************************************
*             初期処理                               1.0
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   SYS-DATE.
     ACCEPT    WK-TIME          FROM   TIME.
*ファイルのＯＰＥＮ
     OPEN     I-O       JHMTANF   DSPFILE.
     OPEN     INPUT     HMEIMS    ZSOKMS.
*ワークの初期化
     INITIALIZE         FLG-AREA.
*ヘッド入力へ
     MOVE    "1"                  TO   PSW.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*
     EVALUATE      PSW
*画面初期化
         WHEN      "1"  PERFORM   DSP-INIT-SEC
*処理区分入力
         WHEN      "2"  PERFORM   DSP-HEAD1-SEC
*キー項目入力
         WHEN      "3"  PERFORM   DSP-HEAD2-SEC
*明細項目入力
         WHEN      "4"  PERFORM   DSP-BODY-SEC
*確認入力
         WHEN      "5"  PERFORM   DSP-KAKU-SEC
*
         WHEN      OTHER  CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             初期画面表示( PSW = 1 )                          *
****************************************************************
 DSP-INIT-SEC          SECTION.
     MOVE     "DSP-INIT-SEC"      TO   S-NAME.
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
*画面の初期化
     MOVE    SPACE                TO   DSP-FKY14011.
*システム日付転送
     MOVE    SYS-DATE(1:4)        TO   G-DATE-YY.
     MOVE    SYS-DATE(5:2)        TO   G-DATE-MM.
     MOVE    SYS-DATE(7:2)        TO   G-DATE-DD.
     MOVE    G-DATE               TO   DSP-SDATE.
*システム時間転送
     MOVE    WK-TIME(1:2)         TO   G-TIME-HH.
     MOVE    WK-TIME(3:2)         TO   G-TIME-MM.
     MOVE    WK-TIME(5:2)         TO   G-TIME-SS.
     MOVE    G-TIME               TO   DSP-STIME.
*プログラムＩＤ
     MOVE    "SKY1401I"           TO   DSP-PGID.
*ＦＯＲＭＩＤ
     MOVE    "FKY14011"           TO   DSP-FORMID.
*リバース，カーソルパーク解除
*倉庫コード
     MOVE    "M"     TO      EDIT-OPTION   OF  DSP-SKCD.
     MOVE    SPACE   TO      EDIT-CURSOR   OF  DSP-SKCD.
*商品コード
     MOVE    "M"     TO      EDIT-OPTION   OF  DSP-SYOCD1.
     MOVE    SPACE   TO      EDIT-CURSOR   OF  DSP-SYOCD1.
*品単１
     MOVE    "M"     TO      EDIT-OPTION   OF  DSP-SYOCD2.
     MOVE    SPACE   TO      EDIT-CURSOR   OF  DSP-SYOCD2.
*品単２
     MOVE    "M"     TO      EDIT-OPTION   OF  DSP-SYOCD3.
     MOVE    SPACE   TO      EDIT-CURSOR   OF  DSP-SYOCD3.
*品単３
     MOVE    "M"     TO      EDIT-OPTION   OF  DSP-SYOCD4.
     MOVE    SPACE   TO      EDIT-CURSOR   OF  DSP-SYOCD4.
*倉庫コードセット
     IF      LINK-SOKCD  NOT =  1
             MOVE    LINK-SOKCD    TO   DSP-SKCD  SOK-F01
             PERFORM ZSOKMS-READ-SEC
             IF  ZSOKMS-INV-FLG = "INV"
                 MOVE ALL NC"＊"   TO   DSP-SKNM
             ELSE
                 MOVE SOK-F02      TO   DSP-SKNM
             END-IF
             MOVE     "X"          TO   EDIT-STATUS OF DSP-SKCD
     ELSE
             MOVE      SPACE       TO   EDIT-STATUS OF DSP-SKCD
     END-IF.
*処理区分入力へ
     MOVE    "2"                  TO   PSW.
*
 DSP-INIT-EXIT.
     EXIT.
****************************************************************
*             処理区分入力( PSW = 2 )                2.1       *
****************************************************************
 DSP-HEAD1-SEC         SECTION.
     MOVE     "DSP-HEAD1-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   HEAD1-CHK-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
         WHEN   OTHER
                MOVE     6       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-HEAD1-EXIT.
     EXIT.
****************************************************************
*             処理区分チェック                       2.1.1     *
****************************************************************
 HEAD1-CHK-SEC             SECTION.
*処理区分 未入力はエラー
     IF       DSP-SHORI      NOT NUMERIC
     OR       DSP-SHORI      =    ZERO
              MOVE      2         TO   ERR-FLG
              GO                  TO   HEAD1-CHK-EXIT
     END-IF.
*処理区分＝１，２，３以外はエラー
     IF    DSP-SHORI    =   1   OR   2   OR   3
              MOVE     "3"        TO   PSW
*             同一モードでループさせるため
              MOVE     DSP-SHORI  TO   SAV-SHORI
     ELSE
              MOVE      2         TO   ERR-FLG
     END-IF.
*
 HEAD1-CHK-EXIT.
     EXIT.
****************************************************************
*             キー項目入力( PSW = 3 )                2.2       *
****************************************************************
 DSP-HEAD2-SEC         SECTION.
     MOVE     "DSP-HEAD2-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   HEAD2-CHK-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                MOVE    "2"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
         WHEN   OTHER
                MOVE     1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-HEAD2-EXIT.
     EXIT.
****************************************************************
*             キー項目チェック                       2.2.1     *
****************************************************************
 HEAD2-CHK-SEC         SECTION.
     MOVE     "HEAD2-CHK-SEC"     TO   S-NAME.
*キー項目 未入力チェック
***  倉庫コード（倉庫マスタのチェック）
*****DISPLAY "SOKCD1 = " DSP-SKCD UPON CONS.
     IF       DSP-SKCD  NOT  NUMERIC
     OR       DSP-SKCD  =    ZERO
              MOVE   3       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION   OF  DSP-SKCD
              MOVE  "C"      TO   EDIT-CURSOR   OF  DSP-SKCD
     ELSE
              MOVE  DSP-SKCD TO   SOK-F01
              PERFORM ZSOKMS-READ-SEC
              IF    ZSOKMS-INV-FLG  =  "INV"
                    MOVE  3  TO   ERR-FLG
                    MOVE "R" TO   EDIT-OPTION   OF  DSP-SKCD
                    MOVE "C" TO   EDIT-CURSOR   OF  DSP-SKCD
              ELSE
                    MOVE SOK-F02 TO DSP-SKNM
                    MOVE "M" TO   EDIT-OPTION   OF  DSP-SKCD
                    MOVE SPACE TO EDIT-CURSOR   OF  DSP-SKCD
              END-IF
*****DISPLAY "SOKCD2 = " DSP-SKCD UPON CONS.
***  商品コードチェック
     IF       DSP-SYOCD1    =    SPACE
              IF     ERR-FLG  =  ZERO
                     MOVE    4    TO    ERR-FLG
              END-IF
              MOVE "R"    TO EDIT-OPTION OF DSP-SYOCD1
              MOVE "R"    TO EDIT-OPTION OF DSP-SYOCD2
              MOVE "R"    TO EDIT-OPTION OF DSP-SYOCD3
              MOVE "R"    TO EDIT-OPTION OF DSP-SYOCD4
              MOVE "C"    TO EDIT-CURSOR OF DSP-SYOCD1
     ELSE
              MOVE  DSP-SYOCD1  TO  MEI-F011
              MOVE  DSP-SYOCD2  TO  MEI-F0121
              MOVE  DSP-SYOCD3  TO  MEI-F0122
              MOVE  DSP-SYOCD4  TO  MEI-F0123
              PERFORM HMEIMS-READ-SEC
              IF   HMEIMS-INV-FLG  =  "INV"
                   IF     ERR-FLG  =  ZERO
                          MOVE    4    TO    ERR-FLG
                   END-IF
                   MOVE "R"    TO EDIT-OPTION OF DSP-SYOCD1
                   MOVE "R"    TO EDIT-OPTION OF DSP-SYOCD2
                   MOVE "R"    TO EDIT-OPTION OF DSP-SYOCD3
                   MOVE "R"    TO EDIT-OPTION OF DSP-SYOCD4
                   MOVE "C"    TO EDIT-CURSOR OF DSP-SYOCD1
              ELSE
                   MOVE "M"    TO EDIT-OPTION OF DSP-SYOCD1
                   MOVE "M"    TO EDIT-OPTION OF DSP-SYOCD3
                   MOVE "M"    TO EDIT-OPTION OF DSP-SYOCD3
                   MOVE "M"    TO EDIT-OPTION OF DSP-SYOCD4
                   MOVE  SPACE TO EDIT-CURSOR OF DSP-SYOCD1
                   MOVE  MEI-F021 TO             DSP-SYONM1
                   MOVE  MEI-F022 TO             DSP-SYONM2
                   MOVE  MEI-F031 TO             DSP-SYOKN1
                   MOVE  MEI-F032 TO             DSP-SYOKN2
                   MOVE  MEI-F06  TO             DSP-JANCD
              END-IF
     END-IF.
*****DISPLAY "SOKCD3 = " DSP-SKCD UPON CONS.
*エラーの場合は、ＥＸＩＴへ
     IF       ERR-FLG  NOT =  ZERO
              GO       TO     HEAD2-CHK-EXIT
     END-IF.
*倉庫別_番マスタ読込み
     MOVE     DSP-SKCD        TO     TAN-F01.
     MOVE     DSP-SYOCD1      TO     TAN-F021.
     MOVE     DSP-SYOCD2      TO     TAN-F022.
     MOVE     DSP-SYOCD3      TO     TAN-F023.
     MOVE     DSP-SYOCD4      TO     TAN-F024.
     PERFORM   JHMTANF-READ-SEC.
*処理区分により判定
     EVALUATE  DSP-SHORI
         WHEN  1
               IF   JHMTANF-INV-FLG = SPACE
                    MOVE  5     TO ERR-FLG
                    MOVE "R"    TO EDIT-OPTION OF DSP-SKCD
                    MOVE "R"    TO EDIT-OPTION OF DSP-SYOCD1
                    MOVE "R"    TO EDIT-OPTION OF DSP-SYOCD2
                    MOVE "R"    TO EDIT-OPTION OF DSP-SYOCD3
                    MOVE "C"    TO EDIT-CURSOR OF DSP-SKCD
               ELSE
                    MOVE "4"    TO PSW
                    MOVE "M"    TO EDIT-OPTION OF DSP-SKCD
                    MOVE "M"    TO EDIT-OPTION OF DSP-SYOCD1
                    MOVE "M"    TO EDIT-OPTION OF DSP-SYOCD2
                    MOVE "M"    TO EDIT-OPTION OF DSP-SYOCD3
                    MOVE  SPACE TO EDIT-CURSOR OF DSP-SKCD
               END-IF
         WHEN  2
               IF   JHMTANF-INV-FLG = "INV"
                    MOVE    6   TO ERR-FLG
                    MOVE "R"    TO EDIT-OPTION OF DSP-SKCD
                    MOVE "R"    TO EDIT-OPTION OF DSP-SYOCD1
                    MOVE "R"    TO EDIT-OPTION OF DSP-SYOCD2
                    MOVE "R"    TO EDIT-OPTION OF DSP-SYOCD3
                    MOVE "C"    TO EDIT-CURSOR OF DSP-SKCD
               ELSE
                    MOVE "4"    TO PSW
                    MOVE "M"    TO EDIT-OPTION OF DSP-SKCD
                    MOVE "M"    TO EDIT-OPTION OF DSP-SYOCD1
                    MOVE "M"    TO EDIT-OPTION OF DSP-SYOCD2
                    MOVE "M"    TO EDIT-OPTION OF DSP-SYOCD3
                    MOVE  SPACE TO EDIT-CURSOR OF DSP-SKCD
                    PERFORM MST-DSP-SEC
               END-IF
         WHEN  3
               IF   JHMTANF-INV-FLG = "INV"
                    MOVE    6   TO ERR-FLG
                    MOVE "R"    TO EDIT-OPTION OF DSP-SKCD
                    MOVE "R"    TO EDIT-OPTION OF DSP-SYOCD1
                    MOVE "R"    TO EDIT-OPTION OF DSP-SYOCD2
                    MOVE "R"    TO EDIT-OPTION OF DSP-SYOCD3
                    MOVE "C"    TO EDIT-CURSOR OF DSP-SKCD
               ELSE
                    MOVE "5"    TO PSW
                    MOVE "M"    TO EDIT-OPTION OF DSP-SKCD
                    MOVE "M"    TO EDIT-OPTION OF DSP-SYOCD1
                    MOVE "M"    TO EDIT-OPTION OF DSP-SYOCD2
                    MOVE "M"    TO EDIT-OPTION OF DSP-SYOCD3
                    MOVE  SPACE TO EDIT-CURSOR OF DSP-SKCD
                    PERFORM MST-DSP-SEC
               END-IF
     END-EVALUATE.
*****DISPLAY "SOKCD4 = " DSP-SKCD UPON CONS.
*
 HEAD2-CHK-EXIT.
     EXIT.
****************************************************************
*             明細項目　入力( PSW = 4 )              2.3       *
****************************************************************
 DSP-BODY-SEC          SECTION.
     MOVE     "DSP-BODY-SEC"      TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                MOVE    "5"      TO   PSW
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                MOVE    "3"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
         WHEN   OTHER
                MOVE     1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-BODY-EXIT.
     EXIT.
****************************************************************
*             確認処理　入力（ PSW = 5 ）            2.4
****************************************************************
 DSP-KAKU-SEC          SECTION.
     MOVE     "DSP-KAKU-SEC"      TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                EVALUATE  DSP-SHORI
*-------------------登録
                    WHEN  "1"
                          PERFORM     FILE-WRT-SEC
*-------------------修正
                    WHEN  "2"
                          PERFORM     FILE-UPD-SEC
*-------------------削除
                    WHEN  "3"
                          PERFORM     FILE-DLT-SEC
                END-EVALUATE
                PERFORM   DSP-INIT-SEC
                MOVE    "3"      TO   PSW
                MOVE   SAV-SHORI TO   DSP-SHORI
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                IF  DSP-SHORI   =  1   OR   2
                    MOVE    "4"       TO   PSW
                ELSE
                    MOVE    "3"       TO   PSW
                END-IF
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
         WHEN   OTHER
                MOVE     1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*    倉庫別_番マスタ更新　　処理区分＝１（登録）2.4.1         *
****************************************************************
 FILE-WRT-SEC           SECTION.
     MOVE     "FILE-WRT-SEC"      TO   S-NAME.
*レコード初期クリア
     MOVE     SPACE               TO   TAN-REC.
     INITIALIZE                        TAN-REC.
*キー項目転送
***  倉庫コード
     MOVE     DSP-SKCD            TO   TAN-F01.
***  商品コード
     MOVE     DSP-SYOCD1          TO   TAN-F021.
***  商品コード（品単１）
     MOVE     DSP-SYOCD2          TO   TAN-F022.
***  商品コード（品単１）
     MOVE     DSP-SYOCD3          TO   TAN-F023.
***  商品コード（品単１）
     MOVE     DSP-SYOCD4          TO   TAN-F024.
***  _番
     MOVE     DSP-TANCD           TO   TAN-F03.
***  更新日付
     MOVE     SYS-DATE            TO   TAN-F98.
***  登録日付
     MOVE     SYS-DATE            TO   TAN-F99.
*倉庫別_番マスタ登録
     WRITE    TAN-REC.
*
 FILE-WRT-EXIT.
     EXIT.
****************************************************************
*    倉庫別_番マスタ更新　　処理区分＝２（修正）  2.4.2       *
****************************************************************
 FILE-UPD-SEC           SECTION.
     MOVE     "FILE-UPD-SEC"      TO   S-NAME.
*キー項目転送
***  倉庫コード
     MOVE     DSP-SKCD            TO   TAN-F01.
***  商品コード
     MOVE     DSP-SYOCD1          TO   TAN-F021.
***  商品コード（品単１）
     MOVE     DSP-SYOCD2          TO   TAN-F022.
***  商品コード（品単１）
     MOVE     DSP-SYOCD3          TO   TAN-F023.
***  商品コード（品単１）
     MOVE     DSP-SYOCD4          TO   TAN-F024.
     PERFORM  JHMTANF-READ-SEC.
     IF       JHMTANF-INV-FLG = SPACE
***           _番
              MOVE     DSP-TANCD  TO   TAN-F03
***           更新日付
              MOVE     SYS-DATE   TO   TAN-F98
***           更新処理
              REWRITE  TAN-REC
     ELSE
              DISPLAY "## JHMTANF REWRITE ERR ##"        UPON CONS
              DISPLAY "## KEY1 (SOKCD) = " TAN-F01 " ##" UPON CONS
              DISPLAY "## KEY2 (SYOCD) = " TAN-F021 "-" TAN-F022
                      "-" TAN-F023 "-" TAN-F024 " ##"    UPON CONS

              STOP  RUN
     END-IF.
*
 FILE-UPD-EXIT.
     EXIT.
****************************************************************
*    スポット受信マスタ更新　処理区分＝３（削除）  2.4.3       *
****************************************************************
 FILE-DLT-SEC           SECTION.
     MOVE     "FILE-DLT-SEC"      TO   S-NAME.
*キー項目転送
***  倉庫コード
     MOVE     DSP-SKCD            TO   TAN-F01.
***  商品コード
     MOVE     DSP-SYOCD1          TO   TAN-F021.
***  商品コード（品単１）
     MOVE     DSP-SYOCD2          TO   TAN-F022.
***  商品コード（品単１）
     MOVE     DSP-SYOCD3          TO   TAN-F023.
***  商品コード（品単１）
     MOVE     DSP-SYOCD4          TO   TAN-F024.
     PERFORM  JHMTANF-READ-SEC.
     IF       JHMTANF-INV-FLG = SPACE
***           レコード削除
              DELETE   JHMTANF
     ELSE
              DISPLAY "## JHMTANF DELETE  ERR ##"        UPON CONS
              DISPLAY "## KEY1 (SOKCD) = " TAN-F01 " ##" UPON CONS
              DISPLAY "## KEY2 (SYOCD) = " TAN-F021 "-" TAN-F022
                      "-" TAN-F023 "-" TAN-F024 " ##"    UPON CONS

              STOP  RUN
     END-IF.
*
 FILE-DLT-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                                     *
****************************************************************
 DSP-WRITE-SEC         SECTION.
     MOVE     "DSP-WRITE-SEC"     TO   S-NAME.
*エラーメッセージセット
     IF    ERR-FLG   =    ZERO
           MOVE    SPACE              TO   DSP-ERRMSG
     ELSE
           MOVE    ERR-MSG-R(ERR-FLG) TO   DSP-ERRMSG
           MOVE    ZERO               TO   ERR-FLG
     END-IF.
*ガイドメッセージの設定
     EVALUATE   PSW
*処理区分
         WHEN   "2"
                MOVE    PF-MSG-R(1)        TO   DSP-PFGAID
*キー項目／ボディー部／確認部
         WHEN   "3"   WHEN   "4"   WHEN   "5"
                MOVE    PF-MSG-R(2)        TO   DSP-PFGAID
         WHEN   OTHER
                MOVE    SPACE              TO   DSP-PFGAID
     END-EVALUATE.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FKY14011"          TO   DSP-FMT.
     WRITE    DSP-FKY14011.
*
 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*             画面読込処理                                     *
****************************************************************
 DSP-READ-SEC          SECTION.
     MOVE     "DSP-READ-SEC"      TO   S-NAME.
*
     MOVE    "NE"                 TO   DSP-PRO.
*
*    MOVE    "SCREEN"             TO   DSP-GRP.
     EVALUATE   PSW
*処理区分
         WHEN   "2"
                MOVE    "INIT"    TO   DSP-GRP
*キー項目
         WHEN   "3"
                MOVE    "HEAD"    TO   DSP-GRP
*明細項目
         WHEN   "4"
                MOVE    "BODY"    TO   DSP-GRP
*確認
         WHEN   "5"
                MOVE    "KAKU"    TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FKY14011"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             倉庫別_番マスタ内容画面セット                   *
****************************************************************
 MST-DSP-SEC              SECTION.
     MOVE   "MST-DSP-SEC"         TO   S-NAME.
*倉庫コード
     MOVE    TAN-F01              TO   DSP-SKCD  SOK-F01.
     PERFORM ZSOKMS-READ-SEC.
     IF      ZSOKMS-INV-FLG = "INV"
             MOVE   ALL NC"＊"    TO   DSP-SKNM
     ELSE
             MOVE   SOK-F02       TO   DSP-SKNM
     END-IF.
*商品コード
     MOVE    TAN-F021             TO   DSP-SYOCD1.
     MOVE    TAN-F022             TO   DSP-SYOCD2.
     MOVE    TAN-F023             TO   DSP-SYOCD3.
     MOVE    TAN-F024             TO   DSP-SYOCD4.
     PERFORM HMEIMS-READ-SEC.
     IF      HMEIMS-INV-FLG = "INV"
             MOVE   ALL NC"＊"    TO   DSP-SYONM1
             MOVE   ALL NC"＊"    TO   DSP-SYONM2
             MOVE   ALL "*"       TO   DSP-SYOKN1
             MOVE   ALL "*"       TO   DSP-SYOKN2
             MOVE   ALL "*"       TO   DSP-JANCD
     ELSE
             MOVE   MEI-F021      TO   DSP-SYONM1
             MOVE   MEI-F022      TO   DSP-SYONM2
             MOVE   MEI-F031      TO   DSP-SYOKN1
             MOVE   MEI-F032      TO   DSP-SYOKN2
             MOVE   MEI-F06       TO   DSP-JANCD
     END-IF.
*_番
     MOVE    TAN-F03              TO   DSP-TANCD.
*
 MST-DSP-EXIT.
     EXIT.
****************************************************************
*             倉庫別_番マスタ読込み                           *
****************************************************************
 JHMTANF-READ-SEC         SECTION.
     MOVE   "JHMTANF-READ-SEC"    TO   S-NAME.
*
     READ    JHMTANF
         INVALID
             MOVE    "INV"        TO   JHMTANF-INV-FLG
         NOT INVALID
             MOVE     SPACE       TO   JHMTANF-INV-FLG
     END-READ.
*
 JHMTANF-READ-EXIT.
     EXIT.
****************************************************************
*               商品名称マスタ読込み                           *
****************************************************************
 HMEIMS-READ-SEC         SECTION.
     MOVE     "HTOKMS-READ-SEC"   TO   S-NAME.
*
     READ    HMEIMS
             INVALID
             MOVE    "INV"        TO   HMEIMS-INV-FLG
             NOT   INVALID
             MOVE     SPACE       TO   HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
****************************************************************
*               倉庫マスタ読込み                               *
****************************************************************
 ZSOKMS-READ-SEC         SECTION.
     MOVE     "ZSOKMS-READ-SEC"   TO   S-NAME.
*
     READ    ZSOKMS
             INVALID
             MOVE    "INV"        TO   ZSOKMS-INV-FLG
             NOT   INVALID
             MOVE     SPACE       TO   ZSOKMS-INV-FLG
     END-READ.
*
 ZSOKMS-READ-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             JHMTANF  HMEIMS  ZSOKMS  DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SKY1401I   END PROGRAM  >>******************

```

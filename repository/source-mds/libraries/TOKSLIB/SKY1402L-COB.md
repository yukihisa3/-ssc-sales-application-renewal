# SKY1402L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SKY1402L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理システム　　　　　　　　　*
*    業務名　　　　　　　：　倉庫別_版マスタ　　　　　        *
*    モジュール名　　　　：　倉庫別_番マスタリスト　　　　　　*
*    作成日／更新日　　　：　99/10/25                          *
*    作成者／更新者　　　：　ＮＡＶ萩原                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SKY1402L.
 AUTHOR.               HAGIWARA.
 DATE-WRITTEN.         99/10/25.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     YA           IS   CHR-2
     YB-21        IS   CHR-21
     YB           IS   CHR-15
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*画面ファイル
*倉庫別_番マスタ
     SELECT  JHMTANF   ASSIGN    TO        JHMTANL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       TAN-F01
                                           TAN-F02
                       FILE      STATUS    TAN-ST.
*名称マスタ
     SELECT  HMEIMS    ASSIGN    TO        MEIMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       MEI-F011
                                           MEI-F012
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
*プリント定義ファイル
     SELECT  PRTFILE   ASSIGN    TO        LP-04-PRTF
                       FILE      STATUS    PRT-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 倉庫別_番マスタ                     *
****************************************************************
 FD  JHMTANF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JHMTANF   OF   XFDLIB
                       JOINING   TAN       AS   PREFIX.
****************************************************************
*    FILE = 名称マスタ                                       *
****************************************************************
 FD  HMEIMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HMEIMS    OF   XFDLIB
                       JOINING   MEI       AS   PREFIX.
****************************************************************
*  FILE= 倉庫マスタ                                            *
****************************************************************
 FD  ZSOKMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ZSOKMS    OF   XFDLIB
                       JOINING   SOK       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FKY14021  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
****************************************************************
*プリントファイル                                              *
****************************************************************
 FD  PRTFILE
     LABEL       RECORD    IS        OMITTED.
 01  PRT-REC.
     03  FILLER            PIC X(200).
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
     03  PRT-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*フラグ領域
 01  FLG-AREA.
     03  READ-FLG                 PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG                  PIC  9(01)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  INV-FLG                  PIC  9(01)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
***  改ページ判定用
     03  WK-SOKOCD                PIC  9(02)  VALUE  ZERO.
***  ラインカウント　
     03  L-CNT                    PIC  9(02)  VALUE  ZERO.
***  ページカウント　
     03  P-CNT                    PIC  9(03)  VALUE  ZERO.
***  プログラムスイッチ（画面遷移制御）
 01  PSW                          PIC  X(01)  VALUE  SPACE.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
*
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
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
         05  FILLER              PIC   N(20)
             VALUE NC"倉庫コードを入力してください".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"倉庫マスタに登録されていません".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"倉庫別_番マスタに登録されていません".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"正しい値を入力してください".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  5   PIC   N(20).
*
 01  FILE-ERR.
     03  TAN-ERR           PIC N(20) VALUE
                     NC"倉庫別_番マスタエラー".
     03  MEI-ERR           PIC N(20) VALUE
                        NC"名称マスタエラー".
     03  SOK-ERR           PIC N(20) VALUE
                        NC"倉庫マスタエラー".
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
     03  PRT-ERR           PIC N(20) VALUE
                        NC"プリントファイルエラー".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*
 01  HD1.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  HD1-01                  PIC  X(08).
     03  FILLER                  PIC  X(36)  VALUE  SPACE.
     03  FILLER                  PIC  N(17)  VALUE
         NC"※※　倉庫別_番マスタリスト　※※"
                                 CHARACTER  TYPE  IS  CHR-21.
     03  FILLER                  PIC  X(16)  VALUE  SPACE.
     03  HD1-02                  PIC  9(04).
     03  FILLER                  PIC  N(01)  VALUE  NC"年"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  HD1-03                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"月"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  HD1-04                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"日"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD1-05                  PIC  ZZ9.
     03  FILLER                  PIC  N(01)  VALUE  NC"頁"
                                 CHARACTER  TYPE  IS  CHR-2.
*
 01  HD2                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  FILLER                  PIC  N(03)  VALUE NC"倉庫：".
     03  HD2-01                  PIC  9(02).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD2-02                  PIC  N(18).
*
 01  HD3.
     03  FILLER                  PIC  X(34)  VALUE  SPACE.
     03  FILLER                  PIC  N(05)  VALUE
                                 NC"商品コード"
                                 CHARACTER   TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(57)  VALUE  SPACE.
     03  FILLER                  PIC  N(02)  VALUE
                                 NC"_番"
                                 CHARACTER   TYPE  IS  CHR-2.
*
 01  SEN                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(18)  VALUE
         NC"──────────────────".
 01  SEN1.
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                  PIC  X(36)  VALUE
         "------------------------------------".
 01  DT1                         CHARACTER  TYPE  IS  CHR-15.
     03  FILLER                  PIC  X(34)  VALUE  SPACE.
     03  DT1-01                  PIC  X(08).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-02                  PIC  X(05).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-03                  PIC  X(02).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-04                  PIC  X(01).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-05                  PIC  N(15).
     03  DT1-06                  PIC  N(15).
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-07                  PIC  X(06).
*
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 TAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMTANF.
     DISPLAY     TAN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TAN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HMEIMS.
     DISPLAY     MEI-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     MEI-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     DISPLAY     SOK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SOK-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     DISPLAY     DSP-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DSP-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTFILE.
     DISPLAY     PRT-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     PRT-ST    UPON      CONS.
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
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*ファイルのＯＰＥＮ
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     JHMTANF   HMEIMS   ZSOKMS.
     OPEN     OUTPUT    PRTFILE.
*ワークの初期化
     INITIALIZE         FLG-AREA.
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
     PERFORM  INIT-DSP-SEC.
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
*パラメタ入力
         WHEN      "1"  PERFORM   DSP-PARA-SEC
*確認入力
         WHEN      "2"  PERFORM   DSP-KAKU-SEC
*リスト出力初期処理
         WHEN      "3"  PERFORM   PRT-INIT-SEC
*リスト出力メイン処理
         WHEN      "4"  PERFORM   PRT-MAIN-SEC
                                  UNTIL READ-FLG = "END"
                        CLOSE     PRTFILE
                        OPEN      OUTPUT    PRTFILE
***                   ワーク初期化
                        MOVE      ZERO  TO  P-CNT     L-CNT
                                            INV-FLG
                        MOVE      SPACE TO  READ-FLG
*
         WHEN      OTHER  CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             パラメタ入力( PSW = 1 )                2.1       *
****************************************************************
 DSP-PARA-SEC         SECTION.
     MOVE     "DSP-PARA-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   PARA-CHK-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     4       TO   ERR-FLG
                GO       TO      DSP-PARA-SEC
     END-EVALUATE.
*
 DSP-PARA-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック                       2.1.1     *
****************************************************************
 PARA-CHK-SEC             SECTION.
     MOVE     "PARA-CHK-SEC"     TO   S-NAME.
*
*開始倉庫コードチェック
***  未入力時，開始倉庫コード＝０
     IF       DSP-SOKCD1   NOT NUMERIC
         OR   DSP-SOKCD1   =  ZERO
              MOVE     ZERO       TO   DSP-SOKCD1
     ELSE
***  倉庫マスタＲＥＡＤ
              MOVE      DSP-SOKCD1     TO   SOK-F01
              READ      ZSOKMS
              INVALID
                     MOVE   2     TO   ERR-FLG
                     MOVE  "R"    TO   EDIT-OPTION  OF  DSP-SOKCD1
                     MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-SOKCD1
                     GO           TO   PARA-CHK-EXIT
              NOT INVALID
                     MOVE  "M"    TO   EDIT-OPTION  OF  DSP-SOKCD1
                     MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-SOKCD1
              END-READ
     END-IF.
*
*終了倉庫コードチェック
***  未入力時，終了倉庫コード＝９９
     IF       DSP-SOKCD2   NOT NUMERIC
        OR    DSP-SOKCD2   =  ZERO
        OR    DSP-SOKCD2   = 99
              MOVE         99     TO   DSP-SOKCD2
     ELSE
***  倉庫マスタＲＥＡＤ
              MOVE      DSP-SOKCD2     TO   SOK-F01
              READ      ZSOKMS
              INVALID
                     MOVE   2     TO   ERR-FLG
                     MOVE  "R"    TO   EDIT-OPTION  OF  DSP-SOKCD2
                     MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-SOKCD2
                     GO           TO   PARA-CHK-EXIT
              NOT INVALID
                     MOVE  "M"    TO   EDIT-OPTION  OF  DSP-SOKCD2
                     MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-SOKCD2
              END-READ
     END-IF.
*
*倉庫コード大小チェック
     IF       DSP-SOKCD1  >  DSP-SOKCD2
              MOVE      5    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-SOKCD1
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-SOKCD2
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-SOKCD1
              GO             TO   PARA-CHK-EXIT
     ELSE
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-SOKCD1
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-SOKCD2
              MOVE     SPACE TO   EDIT-CURSOR  OF  DSP-SOKCD1
     END-IF.
*
*倉庫別_番マスタ存在チェック
     PERFORM  FL-START-SEC.
     IF       INV-FLG   NOT = ZERO
              MOVE      3    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-SOKCD1
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-SOKCD2
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-SOKCD1
              MOVE      ZERO TO   INV-FLG
              GO             TO   PARA-CHK-EXIT
     ELSE
***
                PERFORM  FL-READ-SEC
                IF  READ-FLG      =   "END"
                   MOVE      3    TO   ERR-FLG
                   MOVE     "R"   TO   EDIT-OPTION  OF  DSP-SOKCD1
                   MOVE     "R"   TO   EDIT-OPTION  OF  DSP-SOKCD2
                   MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-SOKCD1
                   MOVE     SPACE TO   READ-FLG
                   GO  TO     PARA-CHK-EXIT
                ELSE
                   IF    DSP-SOKCD1 <=   TAN-F01
                     AND DSP-SOKCD2 >=   TAN-F01
                         MOVE  "M"   TO  EDIT-OPTION OF DSP-SOKCD1
                         MOVE  "M"   TO  EDIT-OPTION OF DSP-SOKCD2
                         MOVE  SPACE TO  EDIT-CURSOR OF DSP-SOKCD1
                   ELSE
                         MOVE   3   TO   ERR-FLG
                         MOVE  "R"  TO   EDIT-OPTION OF DSP-SOKCD1
                         MOVE  "R"  TO   EDIT-OPTION OF DSP-SOKCD2
                         MOVE  "C"  TO   EDIT-CURSOR OF DSP-SOKCD1
                         GO         TO   PARA-CHK-EXIT
                   END-IF
                END-IF
***           END-IF
     END-IF.
     CLOSE    JHMTANF.
     OPEN  INPUT   JHMTANF.
*
     MOVE    "2"       TO    PSW.
*
 PARA-CHK-EXIT.
     EXIT.
****************************************************************
*             確認項目入力( PSW = 2 )                2.2       *
****************************************************************
 DSP-KAKU-SEC         SECTION.
     MOVE     "DSP-KAKU-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                MOVE    "3"      TO   PSW
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
         WHEN   "F006"
                MOVE    "1"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     4       TO   ERR-FLG
                GO       TO      DSP-KAKU-SEC
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*             リスト出力初期処理(PSW = 3)            2.3       *
****************************************************************
 PRT-INIT-SEC                SECTION.
     MOVE     "PRT-INIT-SEC"    TO   S-NAME.
*倉庫別_番マスタＳＴＡＲＴ
     PERFORM  FL-START-SEC.
     IF       INV-FLG   NOT = ZERO
              MOVE      3         TO   ERR-FLG
              MOVE      ZERO      TO   INV-FLG
              MOVE      "1"       TO   PSW
              GO        TO        PRT-INIT-EXIT
     END-IF.
*倉庫別_番マスタＲＥＡＤ
     PERFORM  FL-READ-SEC.
     IF       READ-FLG  =    "END"
              MOVE      3         TO   ERR-FLG
              MOVE      SPACE     TO   READ-FLG
              MOVE      "1"       TO   PSW
              GO        TO        PRT-INIT-EXIT
     END-IF.
*１ページ目ヘッダ出力
     PERFORM  HEAD-WRITE-SEC.
*１件目の倉庫コードを改ページ用ワークへ
     MOVE     TAN-F01   TO   WK-SOKOCD.
*
     MOVE    "4"        TO        PSW.
*
 PRT-INIT-EXIT.
     EXIT.
****************************************************************
*             リスト出力メイン処理( PSW = 4 )        2.4       *
****************************************************************
 PRT-MAIN-SEC                SECTION.
     MOVE     "PRT-MAIN-SEC"    TO   S-NAME.
*
*改ページ判定
     IF       L-CNT     >=   62
              MOVE      SPACE    TO    PRT-REC
              WRITE     PRT-REC  AFTER PAGE
              MOVE      ZERO     TO    L-CNT
              PERFORM   HEAD-WRITE-SEC
     END-IF.
*    ブレイク判定
     IF       TAN-F01   NOT =  WK-SOKOCD
              MOVE      SPACE    TO    PRT-REC
              WRITE     PRT-REC  AFTER PAGE
              MOVE      ZERO     TO    L-CNT
              MOVE      ZERO     TO    P-CNT
              MOVE      TAN-F01  TO    WK-SOKOCD
              PERFORM   HEAD-WRITE-SEC
     END-IF.
*
     PERFORM   BODY-WRITE-SEC
*
     PERFORM  FL-READ-SEC.
*
 PRT-MAIN-EXIT.
     EXIT.
****************************************************************
*             ヘッダ部出力処理                       2.4.1     *
****************************************************************
 HEAD-WRITE-SEC                  SECTION.
     MOVE     "HEAD-WRITE-SEC"    TO   S-NAME.
 HEAD010.
*ページカウント
     ADD      1         TO        P-CNT.
*項目設定
*ヘッダ１
***  ＰＧＩＤ
     MOVE     "SKY1402L"          TO        HD1-01.
***  日付
     MOVE     HEN-DATE-YYYY       TO        HD1-02.
     MOVE     HEN-DATE-MM         TO        HD1-03.
     MOVE     HEN-DATE-DD         TO        HD1-04.
***  ページ_
     MOVE     P-CNT               TO        HD1-05.
*
*ヘッダ２
***  倉庫コード
     MOVE     TAN-F01             TO        HD2-01.
***  倉庫名称
     MOVE     TAN-F01             TO        SOK-F01.
     READ     ZSOKMS
       INVALID
              MOVE      SPACE     TO        HD2-02
       NOT INVALID
              MOVE      SOK-F02   TO        HD2-02
     END-READ.
 HEAD020.
*ヘッダ部出力
     WRITE    PRT-REC      FROM   HD1       AFTER  2.
     WRITE    PRT-REC      FROM   HD2       AFTER  2.
     WRITE    PRT-REC      FROM   SEN       AFTER  1.
     WRITE    PRT-REC      FROM   HD3       AFTER  1.
     WRITE    PRT-REC      FROM   SEN       AFTER  1.
 HEAD030.
*
     ADD      7            TO        L-CNT.
 HEAD-WRITE-EXIT.
     EXIT.
****************************************************************
*             明細部出力処理　                       2.4.2     *
****************************************************************
 BODY-WRITE-SEC              SECTION.
     MOVE     "BODY-WRITE-SEC"    TO   S-NAME.
*
     MOVE     SPACE          TO   DT1.
***  商品コード
     MOVE     TAN-F021       TO   DT1-01.
***  品単１
     MOVE     TAN-F022       TO   DT1-02.
***  品単２　
     MOVE     TAN-F023       TO   DT1-03.
***  品単３
     MOVE     TAN-F024       TO   DT1-04.
***  商品名称
     MOVE     TAN-F02        TO   MEI-F01.
     READ     HMEIMS
       INVALID
              MOVE      SPACE     TO   DT1-05  DT1-06
       NOT INVALID
              MOVE      MEI-F021  TO   DT1-05
              MOVE      MEI-F022  TO   DT1-06
     END-READ.
***  _番
     MOVE     TAN-F03             TO   DT1-07.
*
*明細部出力
*
     WRITE    PRT-REC      FROM   DT1       AFTER  1.
*
     ADD      1            TO        L-CNT.
*
 BODY-WRITE-EXIT.
     EXIT.
****************************************************************
*             画面表示処理                                     *
****************************************************************
 DSP-WRITE-SEC         SECTION.
     MOVE     "DSP-WRITE-SEC"     TO   S-NAME.
*エラーメッセージセット
     IF    ERR-FLG   =    ZERO
           MOVE    SPACE              TO   DSP-MSGSPC
     ELSE
           MOVE    ERR-MSG-R(ERR-FLG) TO   DSP-MSGSPC
           MOVE    ZERO               TO   ERR-FLG
     END-IF.
*ガイドメッセージの設定
     EVALUATE   PSW
***      パラメタ項目
         WHEN   "1"
                MOVE    PF-MSG-R(1)        TO   DSP-FNCSPC
***      確認
         WHEN   "2"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
***      その他
         WHEN   OTHER
                MOVE    SPACE              TO   DSP-FNCSPC
     END-EVALUATE.
*
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FKY14021"          TO   DSP-FMT.
     WRITE    DSP-FKY14021.
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
     EVALUATE   PSW
*パラメタ項目
         WHEN   "1"
                MOVE    "PARGRP"  TO   DSP-GRP
*確認
         WHEN   "2"
                MOVE    "CHKGRP"  TO   DSP-GRP
     END-EVALUATE.
*
     MOVE    "FKY14021"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                                     *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*画面の初期化
     MOVE    SPACE                TO   DSP-FKY14021.
*システム日付転送
     MOVE    HEN-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    HEN-TIME             TO   DSP-STIME.
*項目属性クリア　
     PERFORM      DSP-SYOKI-SEC.
*
 INT-DSP-EXIT.
     EXIT.
****************************************************************
*             画面制御項目初期化                               *
****************************************************************
 DSP-SYOKI-SEC         SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*
*リバース，カーソルパーク解除
***  開始倉庫コード
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SOKCD1.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SOKCD1.
***  終了倉庫コード
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SOKCD2.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SOKCD2.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             倉庫別_番リストＳＴＡＲＴ                       *
****************************************************************
 FL-START-SEC          SECTION.
     MOVE     "FL-START-SEC"      TO   S-NAME.
*
     MOVE     DSP-SOKCD1     TO   TAN-F01.
     START    JHMTANF   KEY  IS   >=   TAN-F01
       INVALID
              MOVE      9    TO   INV-FLG
     END-START.
*
 FL-START-EXIT.
     EXIT.
****************************************************************
*             倉庫別_番リストＲＥＡＤ                         *
****************************************************************
 FL-READ-SEC           SECTION.
     MOVE     "FL-READ-SEC"       TO   S-NAME.
*
     READ     JHMTANF        AT   END
              MOVE     "END"      TO   READ-FLG
              MOVE     "1"        TO   PSW
              GO        TO        FL-READ-EXIT
     END-READ.
*
     IF       TAN-F01   >   DSP-SOKCD2
              MOVE     "END"      TO   READ-FLG
              MOVE     "1"        TO   PSW
     END-IF.
*
 FL-READ-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             JHMTANF  HMEIMS  ZSOKMS  DSPFILE  PRTFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SKY1402L   END PROGRAM  >>******************

```

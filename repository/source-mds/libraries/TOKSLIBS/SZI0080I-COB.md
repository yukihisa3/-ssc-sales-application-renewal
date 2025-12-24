# SZI0080I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SZI0080I.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：（株）サカタのタネ殿　　　　　　　　*
*    業務名　　　　　　　：　在庫ＥＸＣＥＬ連携　　            *
*    モジュール名　　　　：　更新結果一覧　出力指示　　        *
*    作成日／作成者　　　：　2016/07/05  INOUE                 *
*    処理概要　　　　　　：　在庫更新済ＥＸＣＥＬデータの　　　*
*    　　　　　　　　　　　　一覧表出力指示　　　　　　　　　　*
*    変更日／変更者　　　：　2016/07/06  INOUE                 *
*    変更内容　　　　　　：　作業区分追加　G1 G5               *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SZI0080I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         2016/07/05.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*担当者マスタ
     SELECT  HTANMS    ASSIGN    TO        TANMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TAN-F01 TAN-F02
                       FILE      STATUS    TAN-ST.
*画面ファイル
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
*  FILE= 担当者マスタ                                        *
****************************************************************
 FD  HTANMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTANMS    OF   XFDLIB
                       JOINING   TAN       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FZI00801  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  TAN-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*フラグ領域
 01  FLG-AREA.
     03  READ-FLG                 PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  SAKKBN-FLG               PIC  9(01)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
***  モード退避
     03  SAV-SHORI                PIC  9(01)  VALUE  ZERO.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(05)     VALUE " *** ".
     03  S-NAME                   PIC  X(30).
*
*システム日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-DATE                  PIC  9(06)  VALUE  ZERO.
     03  SYS-DATE                 PIC  9(08).
*
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
*取引先チェック
 01  WK-TORICD.
     03  WK-TORI                  PIC   9(08)  VALUE  ZERO.
*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-MSG1.
         05  FILLER               PIC   N(15)
             VALUE NC"_取消　_終了".
     03  PF-MSG2.
         05  FILLER               PIC   N(15)
*            VALUE NC"_取消　_終了　_項目戻し".
             VALUE NC"_取消　_終了".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R   OCCURS   2   PIC   N(15).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(30)
             VALUE NC"作業区分が誤りです".
     03  ERR-MSG2.
         05  FILLER              PIC   N(30)
          VALUE NC"作業区分の何れかに「Ｙ」を入力して下さい".
     03  ERR-MSG3.
         05  FILLER              PIC   N(30)
             VALUE NC"無効キーです".
     03  ERR-MSG4.
         05  FILLER              PIC   N(30)
             VALUE NC"日付を入力して下さい".
     03  ERR-MSG5.
         05  FILLER              PIC   N(30)
             VALUE NC"作業区分が誤りです".
     03  ERR-MSG6.
         05  FILLER              PIC   N(30)
             VALUE NC"担当者コードを入力してください".
     03  ERR-MSG7.
         05  FILLER              PIC   N(30)
             VALUE NC"担当者マスタに登録されていません".
     03  ERR-MSG8.
         05  FILLER              PIC   N(30)
             VALUE NC"　　　　　　　　　　　　".
     03  ERR-MSG9.
         05  FILLER              PIC   N(30)
 VALUE NC"条件に誤りがないか確認し、ＥＮＴＥＲを実行して下さい".
     03  ERR-MSG10.
         05  FILLER              PIC   N(30)
         VALUE NC"作業区分に誤りがあります".
     03  ERR-MSG11               PIC  N(30)  VALUE
              NC"　　　　　　　　　　　　　".
     03  ERR-MSG12               PIC  N(30)  VALUE
              NC"　　　　　　　　　　　　　".
     03  ERR-MSG13               PIC  N(30)  VALUE
              NC"　　　　　　　　　　　　".
     03  ERR-MSG14           PIC  N(30)  VALUE
              NC"　　　　　　　　　".
     03  ERR-MSG15           PIC  N(30)  VALUE
              NC"　　　　　　　".
     03  ERR-MSG16           PIC  N(30)  VALUE
              NC"開始が終了を越えています。".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  16  PIC   N(30).
*
 01  FILE-ERR.
     03  TOK-ERR           PIC N(20) VALUE
                        NC"取引先マスタエラー".
     03  TAN-ERR           PIC N(20) VALUE
                        NC"担当者マスタエラー".
     03  SOK-ERR           PIC N(20) VALUE
                        NC"倉庫マスタエラー".
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
 01  JYOKN-MSG.
     03  CHU-MSG           PIC N(04) VALUE
                        NC"抽出対象".
     03  SAI-MSG           PIC N(04) VALUE
                        NC"再送対象".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
******************************************************************
 LINKAGE           SECTION.
******************************************************************
 01  LINK-OUT-BMNCD           PIC  X(04).
 01  LINK-OUT-TANCD           PIC  X(02).
 01  LINK-OUT-DATEF           PIC  9(08).
 01  LINK-OUT-DATET           PIC  9(08).
 01  LINK-OUT-SKBN1           PIC  X(01).
 01  LINK-OUT-SKBN2           PIC  X(01).
 01  LINK-OUT-SKBN3           PIC  X(01).
 01  LINK-OUT-SKBN4           PIC  X(01).
 01  LINK-OUT-SKBN5           PIC  X(01).
 01  LINK-OUT-SKBN6           PIC  X(01).
*
**************************************************************
 PROCEDURE             DIVISION   USING  LINK-OUT-BMNCD
                                         LINK-OUT-TANCD
                                         LINK-OUT-DATEF
                                         LINK-OUT-DATET
                                         LINK-OUT-SKBN1
                                         LINK-OUT-SKBN2
                                         LINK-OUT-SKBN3
                                         LINK-OUT-SKBN4
                                         LINK-OUT-SKBN5
                                         LINK-OUT-SKBN6.
**************************************************************
 DECLARATIVES.
 TAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTANMS.
     DISPLAY     TAN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TAN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     DISPLAY     DSP-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     DSP-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
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
     MOVE      LINK-OUT-YMD       TO   SYS-DATE.
     ACCEPT    WK-TIME          FROM   TIME.
*ファイルのＯＰＥＮ
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     HTANMS.
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
         WHEN      "9"  PERFORM   DSP-KAKU-SEC
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
                MOVE    "4010"   TO   PROGRAM-STATUS
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     3       TO   ERR-FLG
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
*担当者チェック
     IF   DSP-TANCD    =   " "
          MOVE   6     TO   ERR-FLG
          MOVE  "R"    TO   EDIT-OPTION  OF  DSP-TANCD
          MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-TANCD
          GO           TO   PARA-CHK-EXIT
     ELSE
***  担当者マスタＲＥＡＤ
          MOVE      DSP-TANCD      TO   TAN-F02
          MOVE      DSP-BUMON      TO   TAN-F01
          READ      HTANMS
          INVALID
                 MOVE   7     TO   ERR-FLG
                 MOVE  "R"    TO   EDIT-OPTION  OF  DSP-TANCD
                 MOVE  "R"    TO   EDIT-OPTION  OF  DSP-BUMON
                 MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-TANCD
                 GO           TO   PARA-CHK-EXIT
          NOT INVALID
                 MOVE  "M"    TO   EDIT-OPTION  OF  DSP-TANCD
                 MOVE  "M"    TO   EDIT-OPTION  OF  DSP-BUMON
                 MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-TANCD
                 MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-BUMON
                 MOVE  TAN-F03 TO   DSP-TANNM
          END-READ
     END-IF.
*
*開始日未入力時（初期値セット）
     IF  DSP-DATEF  NOT  NUMERIC
              MOVE     ZERO  TO   DSP-DATEF
     END-IF.
*終了日未入力時（初期値セット）
     IF  DSP-DATET  NOT  NUMERIC
              MOVE  99999999 TO   DSP-DATET
     END-IF.
*範囲大小チェック
     IF   DSP-DATEF  >  DSP-DATET
              IF    ERR-FLG = ZERO
                    MOVE     16    TO   ERR-FLG
              END-IF
              MOVE     "C"   TO   EDIT-CURSOR OF DSP-DATEF
              MOVE     "R"   TO   EDIT-OPTION OF DSP-DATEF
              MOVE     "R"   TO   EDIT-OPTION OF DSP-DATET
              GO   TO   PARA-CHK-EXIT
     ELSE
              MOVE     SPACE TO   EDIT-CURSOR OF DSP-DATEF
              MOVE     "M"   TO   EDIT-OPTION OF DSP-DATEF
              MOVE     "M"   TO   EDIT-OPTION OF DSP-DATET
     END-IF.
*日付論理チェック（開始日）
     IF  DSP-DATEF  NOT =  ZERO
              MOVE    "2"        TO        LINK-IN-KBN
              MOVE   DSP-DATEF   TO        LINK-IN-YMD8
              CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                           LINK-IN-YMD6
                                           LINK-IN-YMD8
                                           LINK-OUT-RET
                                           LINK-OUT-YMD
              IF       LINK-OUT-RET   =    ZERO
                       MOVE SPACE  TO   EDIT-CURSOR OF DSP-DATEF
                       MOVE "M"    TO   EDIT-OPTION OF DSP-DATEF
              ELSE
                       IF  ERR-FLG = ZERO
                           MOVE      4    TO   ERR-FLG
                       END-IF
                       MOVE "C"    TO   EDIT-CURSOR OF DSP-DATEF
                       MOVE "R"    TO   EDIT-OPTION OF DSP-DATEF
                       GO   TO   PARA-CHK-EXIT
              END-IF
     END-IF.
*日付論理チェック（終了日）
     IF   DSP-DATET  NOT =  99999999
              MOVE    "2"        TO        LINK-IN-KBN
              MOVE    DSP-DATET  TO        LINK-IN-YMD8
              CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                           LINK-IN-YMD6
                                           LINK-IN-YMD8
                                           LINK-OUT-RET
                                           LINK-OUT-YMD
              IF       LINK-OUT-RET   =    ZERO
                       MOVE SPACE  TO   EDIT-CURSOR OF DSP-DATET
                       MOVE "M"    TO   EDIT-OPTION OF DSP-DATET
              ELSE
                       IF  ERR-FLG = ZERO
                           MOVE      4    TO   ERR-FLG
                       END-IF
                       MOVE "C"    TO   EDIT-CURSOR OF DSP-DATET
                       MOVE "R"    TO   EDIT-OPTION OF DSP-DATET
                       GO   TO   PARA-CHK-EXIT
              END-IF
     END-IF.
*作業区分（棚移動）チェック
     IF  DSP-SKBN1 = "Y"
         MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SKBN1
         MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-SKBN1
     ELSE
         IF  DSP-SKBN1   = " "
             MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SKBN1
             MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-SKBN1
         ELSE
             MOVE  10       TO   ERR-FLG
             MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SKBN1
             MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SKBN1
*            GO             TO   PARA-CHK-EXIT
         END-IF
     END-IF.
*作業区分（ストック_ＣＨＧ）チェック
     IF  DSP-SKBN2 = "Y"
         MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SKBN2
         MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-SKBN2
     ELSE
         IF  DSP-SKBN2   = " "
             MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SKBN2
             MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-SKBN2
         ELSE
             MOVE  10       TO   ERR-FLG
             MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SKBN2
             MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SKBN2
*            GO             TO   PARA-CHK-EXIT
         END-IF
     END-IF.
*作業区分（製品在庫移動）チェック
     IF  DSP-SKBN3 = "Y"
         MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SKBN3
         MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-SKBN3
     ELSE
         IF  DSP-SKBN3   = " "
             MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SKBN3
             MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-SKBN3
         ELSE
             MOVE  10       TO   ERR-FLG
             MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SKBN3
             MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SKBN3
*            GO             TO   PARA-CHK-EXIT
         END-IF
     END-IF.
*作業区分（廃棄）チェック
     IF  DSP-SKBN4 = "Y"
         MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SKBN4
         MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-SKBN4
     ELSE
         IF  DSP-SKBN4   = " "
             MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SKBN4
             MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-SKBN4
         ELSE
             MOVE  10       TO   ERR-FLG
             MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SKBN4
             MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SKBN4
*            GO             TO   PARA-CHK-EXIT
         END-IF
     END-IF.
*作業区分（セット組）チェック
     IF  DSP-SKBN5 = "Y"
         MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SKBN5
         MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-SKBN5
     ELSE
         IF  DSP-SKBN5   = " "
             MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SKBN5
             MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-SKBN5
         ELSE
             MOVE  10       TO   ERR-FLG
             MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SKBN5
             MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SKBN5
*            GO             TO   PARA-CHK-EXIT
         END-IF
     END-IF.
*作業区分（バラシ）チェック
     IF  DSP-SKBN6 = "Y"
         MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SKBN6
         MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-SKBN6
     ELSE
         IF  DSP-SKBN6   = " "
             MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SKBN6
             MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-SKBN6
         ELSE
             MOVE  10       TO   ERR-FLG
             MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SKBN6
             MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SKBN6
*            GO             TO   PARA-CHK-EXIT
         END-IF
     END-IF.
*作業区分　未入力チェック
     IF  ( DSP-SKBN1 = " " ) AND
         ( DSP-SKBN2 = " " ) AND
         ( DSP-SKBN3 = " " ) AND
         ( DSP-SKBN4 = " " ) AND
         ( DSP-SKBN5 = " " ) AND
         ( DSP-SKBN6 = " " )
           MOVE   2       TO   ERR-FLG
*          MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SKBN4
           MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SKBN1
           GO             TO   PARA-CHK-EXIT
     END-IF.
*
     IF    ERR-FLG  =  0
           MOVE     9     TO   PSW
     END-IF.
*
 PARA-CHK-EXIT.
     EXIT.
****************************************************************
*             確認項目入力( PSW = 9 )                2.2       *
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
                MOVE    DSP-BUMON    TO  LINK-OUT-BMNCD
                MOVE    DSP-TANCD    TO  LINK-OUT-TANCD
                MOVE    DSP-DATEF    TO  LINK-OUT-DATEF
                MOVE    DSP-DATET    TO  LINK-OUT-DATET
                MOVE    DSP-SKBN1    TO  LINK-OUT-SKBN1
                MOVE    DSP-SKBN2    TO  LINK-OUT-SKBN2
                MOVE    DSP-SKBN3    TO  LINK-OUT-SKBN3
                MOVE    DSP-SKBN4    TO  LINK-OUT-SKBN4
                MOVE    DSP-SKBN5    TO  LINK-OUT-SKBN5
                MOVE    DSP-SKBN6    TO  LINK-OUT-SKBN6
                MOVE    "END"    TO   END-FLG
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*項目戻し
         WHEN   "F006"
                MOVE    "1"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
         WHEN   OTHER
                MOVE     6       TO   ERR-FLG
                GO       TO      DSP-KAKU-SEC
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
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
         WHEN   "9"
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
***      その他
         WHEN   OTHER
                MOVE    PF-MSG-R(2)        TO   DSP-FNCSPC
     END-EVALUATE.
*
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FZI00801"          TO   DSP-FMT.
     WRITE    DSP-FZI00801.
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
                MOVE    "GRP001"  TO   DSP-GRP
*オンライン
         WHEN   "2"
                MOVE    "GRP002"  TO   DSP-GRP
*手書項目
         WHEN   "3"
                MOVE    "GRP003"  TO   DSP-GRP
*横持項目
         WHEN   "4"
                MOVE    "GRP004"  TO   DSP-GRP
*確認
         WHEN   "9"
                MOVE    "KAKU"    TO   DSP-GRP
     END-EVALUATE.
*
     MOVE    "FZI00801"           TO   DSP-FMT.
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
     MOVE    SPACE                TO   DSP-FZI00801.
*システム日付転送
     MOVE    SYS-DATE(1:4)        TO   DSP-SDATE(1:4).
     MOVE    "/"                  TO   DSP-SDATE(5:1).
     MOVE    SYS-DATE(5:2)        TO   DSP-SDATE(6:2).
     MOVE    "/"                  TO   DSP-SDATE(8:1).
     MOVE    SYS-DATE(7:2)        TO   DSP-SDATE(9:2).
*システム時間転送
     MOVE    WK-TIME(1:2)         TO   DSP-STIME(1:2).
     MOVE    ":"                  TO   DSP-STIME(3:1).
     MOVE    WK-TIME(3:2)         TO   DSP-STIME(4:2).
     MOVE    ":"                  TO   DSP-STIME(6:1).
     MOVE    WK-TIME(5:2)         TO   DSP-STIME(7:2).
     MOVE    "SZI0080I"           TO   DSP-PGID.
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
***  担当者ＣＤ
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-TANCD.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-TANCD.
***  部門ＣＤ
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-BUMON.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-BUMON.
***  作業区分1
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SKBN1.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SKBN1.
***  作業区分2
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SKBN2.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SKBN2.
***  作業区分3
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SKBN3.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SKBN3.
***  作業区分4
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SKBN4.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SKBN4.
***  作業区分5
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SKBN5.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SKBN5.
***  作業区分6
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SKBN6.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SKBN6.
***  更新日
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-DATEF.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-DATEF.
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-DATET.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-DATET.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE      HTANMS DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SZI0080I   END PROGRAM  >>******************

```

# SBT0160I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBT0160I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　入荷連携サブシステム　　　　　　　*
*    業務名　　　　　　　：　入荷連携データ抽出指示            *
*    モジュール名　　　　：　入荷連携データ抽出指示　　        *
*    作成日／更新日　　　：　12/10/01                          *
*    作成者／更新者　　　：　MIURA                             *
*    処理概要　　　　　　：　入荷連携データの抽出条件指定　　　*
*    更新日／更新者　　　：　　　　　　　　　　　　　　　      *
*    修正概要　　　　　　：　　　　　　　　                    *
*                        ：　　　　　　　　　　　　　　        *
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SBT0160I.
 AUTHOR.               NAV.
 DATE-WRITTEN.         12/10/01.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*倉庫マスタ
     SELECT  ZSOKMS    ASSIGN    TO        ZSOKMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       SOK-F01
                       FILE      STATUS    SOK-ST.
*担当者マスタ
     SELECT  HTANMS    ASSIGN    TO        TANMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TAN-F01 TAN-F02
                       FILE      STATUS    TAN-ST.
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
*  FILE= 倉庫マスタ                                          *
****************************************************************
 FD  ZSOKMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ZSOKMS    OF   XFDLIB
                       JOINING   SOK       AS   PREFIX.
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
                       COPY      FBT01601  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  TAN-ST                   PIC  X(02).
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
         05  FILLER              PIC   N(30)
             VALUE NC"送信区分が誤りです".
     03  ERR-MSG2.
         05  FILLER              PIC   N(30)
          VALUE NC"抽出区分の何れかに（Ｙ）を入力して下さい".
     03  ERR-MSG3.
         05  FILLER              PIC   N(30)
             VALUE NC"無効キーです".
     03  ERR-MSG4.
         05  FILLER              PIC   N(30)
             VALUE NC"連携日（日付）論理エラー".
     03  ERR-MSG5.
         05  FILLER              PIC   N(30)
             VALUE NC"送信区分が誤りです".
     03  ERR-MSG6.
         05  FILLER              PIC   N(30)
             VALUE NC"倉庫コードを入力してください".
     03  ERR-MSG7.
         05  FILLER              PIC   N(30)
             VALUE NC"倉庫マスタに登録されていません".
     03  ERR-MSG8.
         05  FILLER              PIC   N(30)
             VALUE NC"連携日を入力してください".
     03  ERR-MSG9.
         05  FILLER              PIC   N(30)
 VALUE NC"条件に誤りがないか確認し、ＥＮＴＥＲを実行して下さい".
     03  ERR-MSG10.
         05  FILLER              PIC   N(30)
         VALUE NC"抽出区分に誤りがあります".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  10  PIC   N(30).
*
 01  FILE-ERR.
     03  TAN-ERR           PIC N(20) VALUE
                        NC"担当者マスタエラー".
     03  SOK-ERR           PIC N(20) VALUE
                        NC"倉庫マスタエラー".
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
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
 01  LINK-IN-BMNCD           PIC  X(04).
 01  LINK-IN-TANCD           PIC  X(02).
 01  LINK-IN-SOKO            PIC  X(02).
 01  LINK-IN-DSOKO           PIC  X(02).
 01  LINK-OUT-BMNCD          PIC  X(04).
 01  LINK-OUT-TANCD          PIC  X(02).
 01  LINK-OUT-SNDKBN         PIC  X(01).
 01  LINK-OUT-CHUKBNN        PIC  X(01).
 01  LINK-OUT-CHUKBNY        PIC  X(01).
 01  LINK-OUT-SOKO           PIC  X(02).
 01  LINK-OUT-RNKBI          PIC  X(08).
*
**************************************************************
 PROCEDURE             DIVISION   USING  LINK-IN-BMNCD
                       LINK-IN-TANCD LINK-IN-SOKO LINK-IN-DSOKO
                       LINK-OUT-BMNCD LINK-OUT-TANCD
                       LINK-OUT-SNDKBN LINK-OUT-CHUKBNN
                       LINK-OUT-CHUKBNY LINK-OUT-SOKO
                       LINK-OUT-RNKBI.
**************************************************************
 DECLARATIVES.
 TAN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTANMS.
     DISPLAY     TAN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TAN-ST    UPON      CONS.
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
     OPEN     INPUT     HTANMS   ZSOKMS.
*ワークの初期化
     INITIALIZE         FLG-AREA.
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
     MOVE     "SBT0160I"          TO   DSP-PGID.
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
*再送条件入力
         WHEN      "2"  PERFORM   DSP-PARA2-SEC
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
     MOVE  "M"      TO   EDIT-OPTION  OF  DSP-SKB.
     MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-SKB.
     MOVE  "M"      TO   EDIT-OPTION  OF  DSP-NYK.
     MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-NYK.
     MOVE  "M"      TO   EDIT-OPTION  OF  DSP-YKMC.
     MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-YKMC.
     MOVE  "M"      TO   EDIT-OPTION  OF  DSP-RNBI.
     MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-RNBI.
*送信区分チェック
     IF       DSP-SKB   = " "
         MOVE    "新規"   TO   DSP-SKBNM
         MOVE    "X"      TO   EDIT-STATUS  OF  DSP-RNBI
         MOVE    SPACE    TO   DSP-RNBI(1:8)
     ELSE
         IF  DSP-SKB   = "1"
             MOVE    "再送"   TO  DSP-SKBNM
             MOVE    " "      TO  EDIT-STATUS  OF  DSP-RNBI
         ELSE
             MOVE  1        TO   ERR-FLG
             MOVE  "R"      TO   EDIT-OPTION  OF  DSP-SKB
             MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-SKB
             GO             TO   PARA-CHK-EXIT
         END-IF
     END-IF.
*抽出区分（入荷）チェック
     IF  DSP-NYK   = "Y"  OR DSP-NYK   = " "
         CONTINUE
     ELSE
         MOVE  10       TO   ERR-FLG
         MOVE  "R"      TO   EDIT-OPTION  OF  DSP-NYK
         MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-NYK
         GO             TO   PARA-CHK-EXIT
     END-IF.
*抽出区分（横持）チェック
     IF  DSP-YKMC  = "Y"
         CONTINUE
     ELSE
         IF  DSP-YKMC  = " "
             IF  DSP-NYK   = " "
                 MOVE  2        TO   ERR-FLG
                 MOVE  "R"      TO   EDIT-OPTION  OF  DSP-NYK
                 MOVE  "R"      TO   EDIT-OPTION  OF  DSP-YKMC
                 MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-NYK
                 GO             TO   PARA-CHK-EXIT
             END-IF
         ELSE
             MOVE  10       TO   ERR-FLG
             MOVE  "R"      TO   EDIT-OPTION  OF  DSP-YKMC
             MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-YKMC
             GO             TO   PARA-CHK-EXIT
         END-IF
     END-IF.
*
*抽出倉庫コードチェック
     IF   DSP-CHUSOK   =   " "
          MOVE   6     TO   ERR-FLG
          MOVE  "R"    TO   EDIT-OPTION  OF  DSP-CHUSOK
          MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-CHUSOK
          GO           TO   PARA-CHK-EXIT
     ELSE
***  倉庫マスタＲＥＡＤ
          MOVE      DSP-CHUSOK     TO   SOK-F01
          READ      ZSOKMS
          INVALID
                 MOVE   7     TO   ERR-FLG
                 MOVE  "R"    TO   EDIT-OPTION  OF  DSP-CHUSOK
                 MOVE  "C"    TO   EDIT-CURSOR  OF  DSP-CHUSOK
                 GO           TO   PARA-CHK-EXIT
          NOT INVALID
                 MOVE  "M"    TO   EDIT-OPTION  OF  DSP-CHUSOK
                 MOVE  SPACE  TO   EDIT-CURSOR  OF  DSP-CHUSOK
                 MOVE  SOK-F02  TO  DSP-CHUSKN
          END-READ
     END-IF.
     IF       DSP-SKB   = " "
         MOVE    "9"     TO    PSW
         MOVE     9      TO   ERR-FLG
     ELSE
         MOVE    "2"     TO    PSW
     END-IF.
*
 PARA-CHK-EXIT.
     EXIT.
****************************************************************
*             横持条件入力( PSW = 2 )                2.2       *
****************************************************************
 DSP-PARA2-SEC         SECTION.
     MOVE     "DSP-PARA2-SEC"     TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   PARA-CHK2-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "4010"   TO   PROGRAM-STATUS
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                PERFORM   INIT-DSP-SEC
*項目戻し
         WHEN   "F006"
                MOVE    "1"      TO   PSW
         WHEN   OTHER
                MOVE     3       TO   ERR-FLG
                GO       TO      DSP-PARA-SEC
     END-EVALUATE.
*
 DSP-PARA-EXIT.
     EXIT.
****************************************************************
*             パラメタチェック２　                   2.1.1     *
****************************************************************
 PARA-CHK2-SEC             SECTION.
     MOVE     "PARA-CHK2-SEC"     TO   S-NAME.
*
     MOVE  "M"      TO   EDIT-OPTION  OF  DSP-RNBI
     MOVE  SPACE    TO   EDIT-CURSOR  OF  DSP-RNBI
*連携日チェック
***  連携日未入力チェック
     IF       DSP-SKB = "1"
         IF       DSP-RNBI    NOT NUMERIC
             OR   DSP-RNBI    =  ZERO
              MOVE   8       TO   ERR-FLG
              MOVE  "R"      TO   EDIT-OPTION  OF  DSP-RNBI
              MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-RNBI
              GO             TO   PARA-CHK-EXIT
         ELSE
***           連携日論理チェック
              MOVE     "2"            TO   LINK-IN-KBN
              MOVE     ZERO           TO   LINK-IN-YMD6
              MOVE     DSP-RNBI       TO   LINK-IN-YMD8
              MOVE     ZERO           TO   LINK-OUT-RET
              MOVE     ZERO           TO   LINK-OUT-YMD
              CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                              LINK-IN-YMD6
                                              LINK-IN-YMD8
                                              LINK-OUT-RET
                                              LINK-OUT-YMD
              IF   LINK-OUT-RET   = 9
                   MOVE  4        TO   ERR-FLG
                   MOVE  "R"      TO   EDIT-OPTION  OF  DSP-RNBI
                   MOVE  "C"      TO   EDIT-CURSOR  OF  DSP-RNBI
                   GO             TO   PARA-CHK-EXIT
              END-IF
*
         END-IF
     END-IF.
     MOVE    "9"       TO    PSW.
     MOVE     9      TO   ERR-FLG.
*
 PARA-CHK2-EXIT.
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
                MOVE    DSP-SKB       TO  LINK-OUT-SNDKBN
                MOVE    DSP-NYK       TO  LINK-OUT-CHUKBNN
                MOVE    DSP-YKMC      TO  LINK-OUT-CHUKBNY
                MOVE    DSP-CHUSOK    TO  LINK-OUT-SOKO
                MOVE    DSP-RNBI      TO  LINK-OUT-RNKBI
                MOVE    LINK-IN-BMNCD TO  LINK-OUT-BMNCD
                MOVE    LINK-IN-TANCD TO  LINK-OUT-TANCD
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
           MOVE    SPACE              TO   DSP-ERRMSG
     ELSE
           MOVE    ERR-MSG-R(ERR-FLG) TO   DSP-ERRMSG
           MOVE    ZERO               TO   ERR-FLG
     END-IF.
*ガイドメッセージの設定
     EVALUATE   PSW
***      パラメタ項目
         WHEN   "1"
                MOVE    PF-MSG-R(1)        TO   DSP-PFGAID
***      横持条件項目
         WHEN   "2"
                MOVE    PF-MSG-R(2)        TO   DSP-PFGAID
***      確認
         WHEN   "9"
                MOVE    PF-MSG-R(2)        TO   DSP-PFGAID
***      その他
         WHEN   OTHER
                MOVE    SPACE              TO   DSP-PFGAID
     END-EVALUATE.
*
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FBT01601"          TO   DSP-FMT.
     WRITE    DSP-FBT01601.
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
*横持項目
         WHEN   "2"
                MOVE    "GRP002"  TO   DSP-GRP
*確認
         WHEN   "9"
                MOVE    "KAKU"  TO   DSP-GRP
     END-EVALUATE.
*
     MOVE    "FBT01601"           TO   DSP-FMT.
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
     MOVE    SPACE                TO   DSP-FBT01601.
*システム日付転送
     MOVE    SYS-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    WK-TIME(1:6)         TO   DSP-STIME.
*ＰＧＩＤ
     MOVE    "SBT0160I"           TO   DSP-PGID.
*連携担当者
     PERFORM      DSP-TANTO-SEC.
*項目属性クリア　
     PERFORM      DSP-SYOKI-SEC.
*
 INT-DSP-EXIT.
     EXIT.
****************************************************************
*             連携担当者表示                                   *
****************************************************************
 DSP-TANTO-SEC         SECTION.
     MOVE     "DSP-TANTO-SEC"      TO   S-NAME.
*
*担当者マスタ取得
     MOVE      LINK-IN-BMNCD     TO   TAN-F01 DSP-RNTAN1.
*    MOVE      "-"       TO   DSP-RNRAN(5:1).
     MOVE      LINK-IN-TANCD     TO   TAN-F02 DSP-RNTAN2.
     READ      HTANMS
         INVALID  KEY
             MOVE      SPACE     TO   DSP-RNTANN
         NOT INVALID  KEY
             MOVE      TAN-F03(1:10)  TO  DSP-RNTANN
     END-READ.
*
 DSP-TANTO-EXIT.
     EXIT.
****************************************************************
*             画面制御項目初期化                               *
****************************************************************
 DSP-SYOKI-SEC         SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
*
*リバース，カーソルパーク解除
***  送信区分　　　　
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-SKB.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-SKB.
***  抽出区分（入荷）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-NYK.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-NYK.
***  抽出区分（横持）
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-YKMC.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-YKMC.
***  抽出倉庫コード
     IF      LINK-IN-DSOKO  =  "99"
         MOVE      LINK-IN-SOKO  TO   SOK-F01   DSP-CHUSOK
         READ      ZSOKMS
             INVALID  KEY
                 MOVE    SPACE     TO   DSP-CHUSKN
             NOT INVALID  KEY
                 MOVE    SOK-F02   TO   DSP-CHUSKN
         END-READ
         MOVE    "X"    TO   EDIT-STATUS OF DSP-CHUSOK
     ELSE
         MOVE    SPACE  TO    DSP-CHUSOK
         MOVE    "M"    TO  EDIT-OPTION  OF  DSP-CHUSOK
         MOVE    SPACE  TO  EDIT-CURSOR  OF  DSP-CHUSOK
     END-IF.
***  連携日
     MOVE    "X"      TO  EDIT-STATUS  OF  DSP-RNBI.
     MOVE    SPACE    TO  EDIT-CURSOR  OF  DSP-RNBI.
     MOVE    "M"      TO  EDIT-OPTION  OF  DSP-RNBI.
     MOVE    SPACE    TO  DSP-RNBI(1:8).
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             HTANMS  ZSOKMS  DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SBT0160I   END PROGRAM  >>******************

```

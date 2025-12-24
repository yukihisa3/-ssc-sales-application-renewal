# SEG0160I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SEG0160I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　在庫管理サブシステム　　　　　　　*
*    業務名　　　　　　　：　出荷完了　　　　　　　　　　　    *
*    モジュール名　　　　：　受信結果照会                      *
*    作成日／更新日　　　：　00/04/11                          *
*    作成者／更新者　　　：　ＮＡＶ吉田                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SEG0160I.
 AUTHOR.               Y.YOSHIDA.
 DATE-WRITTEN.         00/04/11.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*受信結果ファイル
     SELECT  EGYKEKF   ASSIGN    TO        EGYKEKL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       KEK-F01
                       FILE      STATUS    KEK-ST.
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
*    FILE = 受信結果照会                                       *
****************************************************************
 FD  EGYKEKF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      EGYKEKF   OF   XFDLIB
                       JOINING   KEK       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FEG01601  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  KEK-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*フラグ領域
 01  FLG-AREA.
     03  IX1                      PIC  9(01)  VALUE  ZERO.
     03  INV-FLG                  PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
     03  READ-FLG                 PIC  X(03)  VALUE  SPACE.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(05)     VALUE " *** ".
     03  S-NAME                   PIC  X(30).
*
*システム日付／時刻
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
*受信結果退避
 01  WRK-TBL-AREA.
     03  WRK-TBL                  OCCURS   3.
       05  WRK-DATE.
         07  WRK-DATE-YYYY        PIC   9(04)  VALUE  ZERO.
         07  WRK-DATE-MM          PIC   9(02)  VALUE  ZERO.
         07  WRK-DATE-DD          PIC   9(02)  VALUE  ZERO.
       05  WRK-TIME               PIC   9(06)  VALUE  ZERO.
*
*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-MSG1.
         05  FILLER               PIC   N(15)
             VALUE NC"_終了".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R   OCCURS   1   PIC   N(15).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(20)
             VALUE NC"キーが無効です".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  01  PIC   N(20).
 01  FILE-ERR.
     03  KEK-ERR           PIC N(20) VALUE
                        NC"受信結果ファイルエラー".
     03  DSP-ERR           PIC N(20) VALUE
                        NC"画面ファイルエラー".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 KEK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE EGYKEKF.
     DISPLAY     KEK-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KEK-ST    UPON      CONS.
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
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム時間取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*ファイルのＯＰＥＮ
     OPEN     I-O       DSPFILE.
     OPEN     INPUT     EGYKEKF.
*ワークの初期化
     INITIALIZE         FLG-AREA.
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
*受信結果ファイル読み込み
     PERFORM      EGYKEKF-READ.
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
*初期画面表示
         WHEN      "1"  PERFORM   DSP-INIT-SEC
*確認入力
         WHEN      "4"  PERFORM   DSP-KAKU-SEC
*
         WHEN      OTHER  CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             初期画面表示( PSW = 1 )                2.1       *
****************************************************************
 DSP-INIT-SEC          SECTION.
     MOVE     "DSP-INIT-SEC"      TO   S-NAME.
*画面の初期化
     MOVE    SPACE                TO   DSP-FEG01601.
*システム日付転送
     MOVE    HEN-DATE             TO   DSP-SYSYMD.
*システム時間転送
     MOVE    WK-TIME              TO   DSP-SYSTIM.
*
     MOVE    1                    TO   IX1.
     MOVE    WRK-DATE-YYYY(IX1)   TO   HEN-DATE-YYYY
     MOVE    WRK-DATE-MM  (IX1)   TO   HEN-DATE-MM
     MOVE    WRK-DATE-DD  (IX1)   TO   HEN-DATE-DD
     MOVE    HEN-DATE             TO   DSP-DATE1.
     MOVE    WRK-TIME     (IX1)   TO   DSP-TIME1.
*
     MOVE    2                    TO   IX1.
     MOVE    WRK-DATE-YYYY(IX1)   TO   HEN-DATE-YYYY
     MOVE    WRK-DATE-MM  (IX1)   TO   HEN-DATE-MM
     MOVE    WRK-DATE-DD  (IX1)   TO   HEN-DATE-DD
     MOVE    HEN-DATE             TO   DSP-DATE2.
     MOVE    WRK-TIME     (IX1)   TO   DSP-TIME2.
*
     MOVE    3                    TO   IX1.
     MOVE    WRK-DATE-YYYY(IX1)   TO   HEN-DATE-YYYY
     MOVE    WRK-DATE-MM  (IX1)   TO   HEN-DATE-MM
     MOVE    WRK-DATE-DD  (IX1)   TO   HEN-DATE-DD
     MOVE    HEN-DATE             TO   DSP-DATE3.
     MOVE    WRK-TIME     (IX1)   TO   DSP-TIME3.
*
     MOVE    "4"                  TO   PSW.
*
 INT-DSP-EXIT.
     EXIT.
****************************************************************
*             確認処理　入力（ PSW = 4 ）            2.4
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
                MOVE    "1"      TO   PSW
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
         WHEN   OTHER
                MOVE     1       TO   ERR-FLG
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
*処理区分
         WHEN   "1"
                MOVE    PF-MSG-R(1)        TO   DSP-PFGAID
*確認
         WHEN   "4"
                MOVE    PF-MSG-R(1)        TO   DSP-PFGAID
         WHEN   OTHER
                MOVE    SPACE              TO   DSP-PFGAID
     END-EVALUATE.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FEG01601"          TO   DSP-FMT.
     WRITE    DSP-FEG01601.
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
     EVALUATE   PSW
*確認
         WHEN   "4"
                MOVE    "KAKU"    TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FEG01601"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             受信結果ファイル　　ＲＥＡＤ　                   *
****************************************************************
 EGYKEKF-READ             SECTION.
     MOVE     "EGYKEKF-READ"      TO   S-NAME.
*
     INITIALIZE              WRK-TBL-AREA.
     PERFORM  VARYING   IX1  FROM  1  BY  1  UNTIL  IX1  >  3
         MOVE     ZERO       TO   INV-FLG
         MOVE     IX1        TO   KEK-F01
         READ     EGYKEKF
              INVALID
                  MOVE       9         TO   INV-FLG
              NOT INVALID
                  MOVE       KEK-F02   TO   WRK-DATE(IX1)
                  MOVE       KEK-F03   TO   WRK-TIME(IX1)
         END-READ
     END-PERFORM.
*
 EGYKEKF-READ-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             EGYKEKF  DSPFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SEG0160I   END PROGRAM  >>******************

```

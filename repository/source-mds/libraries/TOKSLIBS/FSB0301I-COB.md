# FSB0301I

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/FSB0301I.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＥＯＳ管理　　　　　　　　        *
*    モジュール名　　　　：　ＦＳＢ０３０１Ｉ                  *
*    作成日／更新日　　　：　2005/10/03 - 10/25 (3)            *
*    作成者／更新者　　　：　ＮＡＶ武井                        *
*    処理概要　　　　　　：　商品ＣＤ検索・サブルーチン        *
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           FSB0301I.
 AUTHOR.               TAKEI.
 DATE-WRITTEN.         05/10/03.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*商品名称マスタ
     SELECT  HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      DYNAMIC
                       RECORD    KEY       MEI-F01
                       FILE      STATUS    MEI-ST.
*商品名称マスタ・カナ名
     SELECT  HMKNMS    ASSIGN    TO        DA-01-VI-MEIMS4
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      DYNAMIC
                       RECORD    KEY       MKN-F031 MKN-F032
                       FILE      STATUS    MKN-ST.
*商品変換テーブル
     SELECT  HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      DYNAMIC
                       RECORD    KEY       STB-F01  STB-F02
                       FILE      STATUS    STB-ST.
*商品変換テーブル４
     SELECT  SHOTBL4   ASSIGN    TO        DA-01-VI-SHOTBL4
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      DYNAMIC
                       RECORD    KEY       SHT-F01  SHT-F031
                                           SHT-F032
                       FILE      STATUS    SHT-ST.
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
*    FILE = 商品名称マスタ                                     *
****************************************************************
 FD  HMEIMS
                       BLOCK     CONTAINS  6    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HMEIMS    OF   XFDLIB
                       JOINING   MEI       AS   PREFIX.
****************************************************************
*    FILE = 商品名称マスタ・カナ名                             *
****************************************************************
 FD  HMKNMS
                       BLOCK     CONTAINS  6    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HMEIMS    OF   XFDLIB
                       JOINING   MKN       AS   PREFIX.
****************************************************************
*    FILE = 商品変換テーブル                                   *
****************************************************************
 FD  HSHOTBL
                       BLOCK     CONTAINS  12    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HSHOTBL   OF   XFDLIB
                       JOINING   STB       AS   PREFIX.
****************************************************************
*    FILE = 商品変換テーブル４                                 *
****************************************************************
 FD  SHOTBL4
                       BLOCK     CONTAINS  12    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HSHOTBL   OF   XFDLIB
                       JOINING   SHT       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FSUB0301  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  MEI-ST                   PIC  X(02).
     03  MKN-ST                   PIC  X(02).
     03  STB-ST                   PIC  X(02).
     03  SHT-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
 01  SET-PGID                     PIC  X(08)  VALUE "FSB0301I".
 01  SET-FORMID                   PIC  X(08)  VALUE "FSUB0301".
 01  MAX-PGCNT                    PIC  9(02)  VALUE 10.
 01  MAX-LNCNT                    PIC  9(02)  VALUE 07.
*フラグ領域
 01  FLG-AREA.
     03  CHK-FLG                  PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG                  PIC  9(01)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  RD-FLG                   PIC  X(03)  VALUE  SPACE.
     03  SET-FLG                  PIC  X(03)  VALUE  SPACE.
     03  INV-FLG                  PIC  X(03)  VALUE  SPACE.
     03  PG-CNT                   PIC  9(02)  VALUE  ZERO.
     03  S-CNT                    PIC  9(01)  VALUE  ZERO.
     03  C-CNT                    PIC  9(01)  VALUE  ZERO.
     03  X                        PIC  9(02)  VALUE  ZERO.
     03  Y                        PIC  9(02)  VALUE  ZERO.
     03  WK-SEQ                   PIC  9(02)  VALUE  ZERO.
     03  END-SW                   PIC  X(01)  VALUE  SPACE.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
**
     03  WK-TORICD                PIC  9(08)  VALUE  ZERO.
     03  WK-HYOJI                 PIC  9(01).
     03  WK-SSHO.
       05  WK-SSHO1               PIC  X(08).
       05  WK-SSHO2.
         07  WK-SSHO21            PIC  X(05).
         07  WK-SSHO22            PIC  X(02).
         07  WK-SSHO23            PIC  X(01).
     03  WK-HSHO                  PIC  X(13).
     03  WK-SHOKN                 PIC  X(15).
**
     03  WK-DSP1-SHOCD.
       05  WK-DSP1-SSHO1          PIC  X(08).
       05  WK-DSP1-01             PIC  X(01).
       05  WK-DSP1-SSHO21         PIC  X(05).
       05  WK-DSP1-02             PIC  X(01).
       05  WK-DSP1-SSHO22         PIC  X(02).
       05  WK-DSP1-03             PIC  X(01).
       05  WK-DSP1-SSHO23         PIC  X(01).
     03  WK-DSP1-SHON.
       05  WK-DSP1-SHON01         PIC  N(15).
       05  WK-DSP1-SHON02         PIC  N(09).
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
         05  FILLER               PIC   N(20)
      VALUE NC"_取消_終了　　　　　　　　　　　".
     03  PF-MSG2.
         05  FILLER               PIC   N(20)
      VALUE NC"_取消_終了_項目戻り_前頁_次頁".
     03  PF-MSG3.
         05  FILLER               PIC   N(20)
      VALUE NC"_取消_終了_項目戻り　　　　　　".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R   OCCURS   3   PIC   N(20).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(20)
             VALUE NC"無効ＰＦキーです。".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"前頁はありません。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"次頁はありません。".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"選択番号を入力して下さい。".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"選択した番号は対象ではありません。".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"表示順を正しく入力して下さい。".
     03  ERR-MSG7.
         05  FILLER              PIC   N(20)
             VALUE NC"対象データがありません。".
     03  ERR-MSG8.
         05  FILLER              PIC   N(20)
             VALUE NC"　　　　　　　　　　　　".
     03  ERR-MSG9.
         05  FILLER              PIC   N(20)
             VALUE NC"　　　　　　　　　　　　".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  9   PIC   N(20).
*商品データ退避エリア
 01  TABLE-AREA.
         05  TABLE2  OCCURS  07.
             07  TBL-SEQ          PIC   9(02).
             07  TBL-SHOCD        PIC   X(19).
             07  TBL-SHON         PIC   N(24).
             07  TBL-HSHOCD       PIC   X(13).
             07  TBL-TANABN       PIC   X(06).
             07  TBL-SHOK1        PIC   X(15).
             07  TBL-SHOK2        PIC   X(15).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  MEI-ERR           PIC N(15) VALUE
                        NC"商品名称マスタエラー".
     03  MKN-ERR           PIC N(15) VALUE
                        NC"商品名称カナ名エラー".
     03  STB-ERR           PIC N(15) VALUE
                        NC"商品変換テーブルエラー".
     03  SHT-ERR           PIC N(15) VALUE
                        NC"商品変換テーブル４エラー".
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
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
**************************************************************
 LINKAGE               SECTION.
 01  LINK-AREA.
   03  LINK-HYOJI            PIC 9(01).
   03  LINK-TORICD           PIC 9(08).
   03  LINK-SSHO1            PIC X(08).
   03  LINK-SSHO2            PIC X(08).
   03  LINK-HSHO1            PIC X(08).
   03  LINK-HSHO2            PIC X(05).
*
**************************************************************
 PROCEDURE             DIVISION USING  LINK-AREA.
**************************************************************
 DECLARATIVES.
 MEI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HMEIMS.
     MOVE        MEI-ST    TO        E-ST.
     MOVE        "HMEIMS"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     MEI-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 MKN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HMKNMS.
     MOVE        MKN-ST    TO        E-ST.
     MOVE        "HMKNMS"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     MKN-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 STB-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HSHOTBL.
     MOVE        STB-ST    TO        E-ST.
     MOVE        "HSHOTBL"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     STB-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SHT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SHOTBL4.
     MOVE        SHT-ST    TO        E-ST.
     MOVE        "SHOTBL4"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SHT-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 DSP-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     MOVE        DSP-ST    TO        E-ST.
     MOVE        "DSPFILE" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     DSP-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC          UNTIL   END-FLG   =   "END".
     PERFORM   END-SEC.
***
     EXIT  PROGRAM.
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
     OPEN      I-O              DSPFILE.
     OPEN      INPUT   HMEIMS  HMKNMS  HSHOTBL SHOTBL4.
*ワークの初期化
     INITIALIZE                 FLG-AREA.
*初期画面の表示
     MOVE      SPACE              TO   DSP-PRO.
***##MOVE      2                  TO   WK-HYOJI.
     MOVE      LINK-HYOJI         TO   WK-HYOJI.
     MOVE      LINK-TORICD        TO   WK-TORICD.
     PERFORM   INIT-DSP-SEC.
*処理区分入力へ
     MOVE    "1"                  TO   PSW.
*頁セット
     MOVE     1                   TO   PG-CNT.
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
*処理区分入力(2.1)
         WHEN      "1"       PERFORM   DSP-HEAD-SEC
*選択番号入力(2.2)
         WHEN      "2"       PERFORM   DSP-BODY-SEC
*確認入力    (2.3)
         WHEN      "3"       PERFORM   DSP-KAKU-SEC
*メインへ遷移(2.4)
         WHEN      "4"       PERFORM   RETURN-SEC
*以外
         WHEN      OTHER     CONTINUE
     END-EVALUATE.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"          TO   S-NAME.
** 全画面クリア
     MOVE    SPACE     TO   DSP-PRO.
     MOVE    SPACE     TO   DSP-FSUB0301.
     MOVE    "SCREEN"  TO   DSP-GRP.
     MOVE    "FSUB0301" TO  DSP-FMT.
     MOVE    "CL"      TO   DSP-PRO.
     WRITE   DSP-FSUB0301.
*ファイル ＣＬＯＳＥ
     CLOSE   DSPFILE.
     CLOSE   HMEIMS  HMKNMS  HSHOTBL SHOTBL4.
**PF05 ﾓﾄﾞﾘ
     IF  END-SW  = "Y"
         MOVE   SPACE      TO    LINK-SSHO1  LINK-SSHO2
                                 LINK-HSHO1  LINK-HSHO2
     END-IF.
**
 END-EXIT.
     EXIT.
****************************************************************
*           処理区分入力(PSW=1)                          2.1   *
****************************************************************
 DSP-HEAD-SEC          SECTION.
     MOVE     "DSP-HEAD-SEC"      TO   S-NAME.
*
     PERFORM  DSP-WRITE-SEC.
     PERFORM  DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
       WHEN   "E000"
           IF  DSP-HYOJI  =  1 OR 2 OR 3
               MOVE  DSP-HYOJI  TO  WK-HYOJI
               MOVE  LINK-TORICD  TO  WK-TORICD
               PERFORM   MST-WORK-SEC
               IF  ERR-FLG  =  ZERO
                   MOVE  "2"   TO   PSW
                   MOVE  "M"   TO  EDIT-OPTION   OF DSP-HYOJI
                   MOVE   SPACE  TO  EDIT-CURSOR OF DSP-HYOJI
                   PERFORM     WORK-DSP-SEC
               END-IF
           ELSE
              MOVE   6       TO  ERR-FLG
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-HYOJI
              MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-HYOJI
           END-IF
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                MOVE     1       TO   PG-CNT
                PERFORM  INIT-DSP-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "Y  "    TO   END-SW
         WHEN   OTHER
                MOVE     1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-HEAD-EXIT.
     EXIT.
****************************************************************
*             選択番号入力( PSW = 2 )                2.2       *
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
                PERFORM   BODY-CHK-SEC
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                MOVE     1       TO   PG-CNT
                PERFORM  INIT-DSP-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "Y"      TO   END-SW
*項目戻し
         WHEN   "F006"
                MOVE    "1"      TO   PSW
                MOVE     1       TO   PG-CNT
                MOVE  "M"      TO  EDIT-OPTION OF DSP-SENNO
                MOVE   SPACE   TO  EDIT-CURSOR OF DSP-SENNO
*前頁
         WHEN   "F011"
                PERFORM  DSP-UP-SEC
*次頁
         WHEN   "F012"
                PERFORM  DSP-DOWN-SEC
         WHEN   OTHER
                MOVE     1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-BODY-EXIT.
     EXIT.
****************************************************************
*             商品選択チェック                       2.2.1     *
****************************************************************
 BODY-CHK-SEC             SECTION.
     MOVE     "BODY-CHK-SEC"     TO   S-NAME.
*選択番号入力
     IF       DSP-SENNO   NOT   NUMERIC
     OR       DSP-SENNO   =     ZERO
              MOVE   4       TO  ERR-FLG
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SENNO
              MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-SENNO
         GO  TO    BODY-CHK-EXIT
     ELSE
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SENNO
              MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-SENNO
     END-IF.
*商品選択チェック
     MOVE     SPACE          TO  CHK-FLG.
     PERFORM VARYING Y FROM 1 BY 1  UNTIL  Y  >  MAX-LNCNT
                                         OR CHK-FLG = "CHK"
          IF  DSP-SENNO = TBL-SEQ(Y)
               MOVE    "CHK"       TO       CHK-FLG
               IF  TBL-SHOCD(Y)  NOT = SPACE
                   MOVE TBL-SHOCD(Y)     TO WK-DSP1-SHOCD
                   MOVE WK-DSP1-SSHO1    TO WK-SSHO1
                   MOVE WK-DSP1-SSHO21   TO WK-SSHO21
                   MOVE WK-DSP1-SSHO22   TO WK-SSHO22
                   MOVE WK-DSP1-SSHO23   TO WK-SSHO23
                   MOVE TBL-HSHOCD(Y)    TO WK-HSHO
                   MOVE  "M"   TO  EDIT-OPTION OF DSP-SENNO
                   MOVE  SPACE TO  EDIT-CURSOR OF DSP-SENNO
               ELSE
                   MOVE  5  TO ERR-FLG
                   MOVE "R" TO EDIT-OPTION OF DSP-SENNO
                   MOVE "C" TO EDIT-CURSOR OF DSP-SENNO
               END-IF
          END-IF
     END-PERFORM.
*
     IF  CHK-FLG  =  SPACE
          MOVE  5  TO ERR-FLG
          MOVE "R" TO EDIT-OPTION OF DSP-SENNO
          MOVE "C" TO EDIT-CURSOR OF DSP-SENNO
     END-IF.
     IF  ERR-FLG = ZERO
         PERFORM  WORK-DSP-SEC
         MOVE  "3"     TO   PSW
     END-IF.
**
 BODY-CHK-EXIT.
     EXIT.
****************************************************************
*             マスタ→ワーク（退避）                 2.1.1     *
****************************************************************
 MST-WORK-SEC          SECTION.
     MOVE     "MST-WORK-SEC"   TO   S-NAME.
*ヘッド部退避
     MOVE    DSP-SSHO1            TO   WK-SSHO1.
     MOVE    DSP-SSHO2            TO   WK-SSHO21.
     MOVE    DSP-SSHO3            TO   WK-SSHO22.
     MOVE    DSP-SSHO4            TO   WK-SSHO23.
     MOVE    DSP-HSHO             TO   WK-HSHO.
     MOVE    DSP-SHOKN            TO   WK-SHOKN.
*ＳＥＱ番号セット
     MOVE      ZERO     TO    WK-SEQ.
     MOVE      SPACE    TO    RD-FLG.
** テーブルクリア
     PERFORM VARYING Y FROM 1 BY 1  UNTIL  Y  >  MAX-LNCNT
             INITIALIZE     TABLE2(Y)
     END-PERFORM.
*
     IF  WK-HYOJI  =  1
         GO  TO  MST-WORK-000
     END-IF.
     IF  WK-HYOJI  =  2
         GO  TO  MST-WORK-020
     END-IF.
     IF  WK-HYOJI  =  3
         GO  TO  MST-WORK-030
     END-IF.
     GO TO  MST-WORK-990.
 MST-WORK-000.
     IF  LINK-TORICD  NOT = ZERO
         GO  TO   MST-WORK-010
     END-IF.
*商品マスタ位置付け：自社コード
*
     MOVE     WK-SSHO1      TO  MEI-F011.
     MOVE     WK-SSHO2      TO  MEI-F012.
     START    HMEIMS    KEY  IS  >= MEI-F01
          INVALID
              MOVE  "END"   TO  RD-FLG
              MOVE  7       TO  ERR-FLG

     END-START.
     GO  TO   MST-WORK-040.
**
 MST-WORK-010.
*商品変換テーブル位置付け：自社コード
     MOVE     WK-TORICD     TO  SHT-F01.
     MOVE     WK-SSHO1      TO  SHT-F031.
     MOVE     WK-SSHO2      TO  SHT-F032.
     START    SHOTBL4   KEY  IS  >= SHT-F01  SHT-F031  SHT-F032
          INVALID
              MOVE  "END"   TO  RD-FLG
              MOVE  7       TO  ERR-FLG

     END-START.
     GO  TO   MST-WORK-040.
 MST-WORK-020.
*商品変換テーブル位置付け：相手先コード
     MOVE     WK-TORICD     TO  STB-F01.
     MOVE     WK-HSHO       TO  STB-F02.
     START    HSHOTBL    KEY  IS  >= STB-F01  STB-F02
          INVALID
              MOVE  "END"    TO  RD-FLG
              MOVE   7       TO  ERR-FLG

     END-START.
     GO  TO   MST-WORK-040.
 MST-WORK-030.
*商品マスタ・カナ名位置付け
     MOVE     WK-SHOKN      TO  MKN-F031.
     MOVE     SPACE         TO  MKN-F032.
     START    HMKNMS    KEY  IS  >= MKN-F031  MKN-F032
          INVALID
              MOVE  "END"    TO  RD-FLG
              MOVE   7       TO  ERR-FLG

     END-START.
     GO  TO   MST-WORK-040.
 MST-WORK-040.
     IF  RD-FLG  =  "END"
         GO  TO  MST-WORK-990
     END-IF.
*マスタを読込みワークへセット
     PERFORM VARYING Y FROM 1 BY 1  UNTIL  Y  >  MAX-LNCNT
                                   OR RD-FLG = "END"
             PERFORM  MST-READ-SEC
             IF  RD-FLG NOT = "END"
                      MOVE  MEI-F011       TO  WK-DSP1-SSHO1
                      MOVE  MEI-F0121      TO  WK-DSP1-SSHO21
                      MOVE  MEI-F0122      TO  WK-DSP1-SSHO22
                      MOVE  MEI-F0123      TO  WK-DSP1-SSHO23
                      MOVE  "-"            TO  WK-DSP1-01
                            WK-DSP1-02  WK-DSP1-03
                      MOVE  WK-DSP1-SHOCD  TO  TBL-SHOCD(Y)
                      MOVE  MEI-F021       TO  WK-DSP1-SHON01
                      MOVE  MEI-F022       TO  WK-DSP1-SHON02
                      MOVE  WK-DSP1-SHON   TO  TBL-SHON(Y)
                      MOVE  STB-F02        TO  TBL-HSHOCD(Y)
                      MOVE  STB-F08        TO  TBL-TANABN(Y)
                      MOVE  MEI-F031       TO  TBL-SHOK1(Y)
                      MOVE  MEI-F032       TO  TBL-SHOK2(Y)
                      ADD   1   TO   WK-SEQ
                      MOVE  WK-SEQ         TO  TBL-SEQ(Y)
             END-IF
     END-PERFORM.
**
     IF  WK-SEQ  =  ZERO
         MOVE    7   TO  ERR-FLG
     END-IF.
 MST-WORK-990.
**
 MST-WORK-EXIT.
     EXIT.
****************************************************************
*             ワーク→画面表示                       2.1.2     *
****************************************************************
 WORK-DSP-SEC          SECTION.
     MOVE     "WORK-DSP-SEC"      TO   S-NAME.
*項目画面セット
     MOVE    WK-HYOJI             TO   DSP-HYOJI.
     MOVE    WK-SSHO1             TO   DSP-SSHO1.
     MOVE    WK-SSHO21            TO   DSP-SSHO2.
     MOVE    WK-SSHO22            TO   DSP-SSHO3.
     MOVE    WK-SSHO23            TO   DSP-SSHO4.
     MOVE    WK-HSHO              TO   DSP-HSHO.
     MOVE    WK-SHOKN             TO   DSP-SHOKN.
*
     PERFORM VARYING Y FROM 1 BY 1 UNTIL Y  >  MAX-LNCNT
         IF  TBL-SHOCD(Y)  NOT =  SPACE
             MOVE    TBL-SEQ(Y)    TO  DSP-SEQ(Y)
             MOVE    TBL-SHOCD(Y)  TO  DSP-SHOCD(Y)
             MOVE    TBL-SHON(Y)   TO  DSP-SHON(Y)
             MOVE    TBL-HSHOCD(Y) TO  DSP-HSHOCD(Y)
             MOVE    TBL-TANABN(Y) TO  DSP-TANAB(Y)
             MOVE    TBL-SHOK1(Y)  TO  DSP-SHOK1(Y)
             MOVE    TBL-SHOK2(Y)  TO  DSP-SHOK2(Y)
         ELSE
             MOVE    SPACE         TO  DSP-BDY(Y)
         END-IF
     END-PERFORM.
*
 WORK-DSP-EXIT.
     EXIT.
****************************************************************
*             確認処理　入力（ PSW = 3 ）             2.3
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
                MOVE    "4"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
                MOVE     1       TO   PG-CNT
                PERFORM  INIT-DSP-SEC
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
                MOVE    "Y"      TO   END-SW
*項目戻し
         WHEN   "F006"
                MOVE    "2"      TO   PSW
         WHEN   OTHER
                MOVE     1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*             メインへ戻り値                          2.4      *
****************************************************************
 RETURN-SEC              SECTION.
     MOVE     "RETURNT-SEC"  TO   S-NAME.
*選択商品表示
*****DISPLAY NC"サカタ商品ＣＤ＝" WK-SSHO1    UPON CONS.
*****DISPLAY NC"サカタ品番ＣＤ＝" WK-SSHO2    UPON CONS.
*****DISPLAY NC"相手商品ＣＤ１＝" WK-HSHO(1:8)  UPON CONS.
*****DISPLAY NC"相手商品ＣＤ２＝" WK-HSHO(9:5)  UPON CONS.
**
*処理終了へ
     MOVE   WK-SSHO1       TO         LINK-SSHO1.
     MOVE   WK-SSHO21      TO         LINK-SSHO2(1:5).
     MOVE   WK-SSHO22      TO         LINK-SSHO2(6:2).
     MOVE   WK-SSHO23      TO         LINK-SSHO2(8:1).
     MOVE   WK-HSHO(1:8)   TO         LINK-HSHO1.
     MOVE   WK-HSHO(9:5)   TO         LINK-HSHO2.
     MOVE    "END"         TO         END-FLG.
*
 RETURN-EXIT.
     EXIT.
****************************************************************
*             初期画面表示                               1.1   *
****************************************************************
 INIT-DSP-SEC          SECTION.
     MOVE     "INIT-DSP-SEC"      TO   S-NAME.
 INITD001.
*画面の初期化
     MOVE    SPACE                TO   DSP-FSUB0301.
 INITD003.
*システム日付・時間転送
     MOVE    HEN-DATE             TO   DSP-SDATE.
***##MOVE    SYS-DATE             TO   DSP-SDATE.
     MOVE    HEN-TIME(1:5)        TO   DSP-STIME.
***##MOVE    WK-TIME(1:4)         TO   DSP-STIME.
 INITD004.
*ＰＧＩＤ・ＦＭＴＩＤセット
     MOVE    SET-PGID             TO   DSP-PGID.
     MOVE    SET-FORMID           TO   DSP-FORM.
 INITD011.
*項目属性クリア
     PERFORM  DSP-SYOKI-SEC.
 INITD012.
     MOVE    LINK-HYOJI           TO   DSP-HYOJI.
     MOVE    LINK-SSHO1           TO   DSP-SSHO1.
     MOVE    LINK-SSHO2(1:5)      TO   DSP-SSHO2.
     MOVE    LINK-SSHO2(6:2)      TO   DSP-SSHO3.
     MOVE    LINK-SSHO2(8:1)      TO   DSP-SSHO4.
     MOVE    LINK-HSHO1           TO   DSP-HSHO(1:8).
     MOVE    LINK-HSHO2           TO   DSP-HSHO(9:5).
**
*
 INIT-DSP-EXIT.
     EXIT.
****************************************************************
*    次頁処理(PF12)                                     3.1    *
****************************************************************
 DSP-DOWN-SEC          SECTION.
     MOVE     "DSP-DOWN-SEC"   TO   S-NAME.
*ＳＥＱ番号セット
     MOVE      ZERO     TO    WK-SEQ.
     MOVE      SPACE    TO    RD-FLG.
*
     IF  WK-HYOJI  =  1
         GO  TO  DSP-DOWN-000
     END-IF.
     IF  WK-HYOJI  =  2
         GO  TO  DSP-DOWN-020
     END-IF.
     IF  WK-HYOJI  =  3
         GO  TO  DSP-DOWN-030
     END-IF.
     GO TO  DSP-DOWN-990.
 DSP-DOWN-000.
     IF  LINK-TORICD  NOT = ZERO
         GO  TO   DSP-DOWN-010
     END-IF.
*商品マスタ位置付け：自社コード
     IF  TBL-SEQ(MAX-LNCNT) =  ZERO  AND
         TBL-SHOCD(MAX-LNCNT) = SPACE
             MOVE   3       TO   ERR-FLG
             MOVE  "END"    TO   RD-FLG
         GO  TO  DSP-DOWN-990
     END-IF.
     MOVE     TBL-SHOCD(MAX-LNCNT)    TO  WK-DSP1-SHOCD.
     MOVE     WK-DSP1-SSHO1     TO  MEI-F011.
     MOVE     WK-DSP1-SSHO21    TO  MEI-F0121.
     MOVE     WK-DSP1-SSHO22    TO  MEI-F0122.
     MOVE     WK-DSP1-SSHO23    TO  MEI-F0123.
     START    HMEIMS    KEY  IS  >  MEI-F01
          INVALID
              MOVE  "END"   TO  RD-FLG
              MOVE   3      TO  ERR-FLG
              GO  TO    DSP-DOWN-990
     END-START.
     GO  TO   DSP-DOWN-040.
**
 DSP-DOWN-010.
*商品変換テーブル位置付け：自社コード
     IF  TBL-SEQ(MAX-LNCNT) =  ZERO  AND
         TBL-SHOCD(MAX-LNCNT) = SPACE
             MOVE   3       TO   ERR-FLG
             MOVE  "END"    TO   RD-FLG
         GO  TO  DSP-DOWN-990
     END-IF.
     MOVE     WK-TORICD         TO  SHT-F01.
     MOVE     TBL-SHOCD(MAX-LNCNT)    TO  WK-DSP1-SHOCD.
     MOVE     WK-DSP1-SSHO1     TO  SHT-F031.
     MOVE     WK-DSP1-SSHO21    TO  SHT-F032(1:5).
     MOVE     WK-DSP1-SSHO22    TO  SHT-F032(6:2).
     MOVE     WK-DSP1-SSHO23    TO  SHT-F032(8:1).
     START    SHOTBL4   KEY  IS  >  SHT-F01  SHT-F031  SHT-F032
          INVALID
              MOVE  "END"   TO  RD-FLG
              MOVE   3      TO  ERR-FLG
              GO  TO    DSP-DOWN-990
     END-START.
     GO  TO   DSP-DOWN-040.
**
 DSP-DOWN-020.
*商品変換テーブル位置付け：相手先コード
     IF  TBL-SEQ(MAX-LNCNT) =  ZERO  AND
         TBL-HSHOCD(MAX-LNCNT) = SPACE
             MOVE     3    TO  ERR-FLG
             MOVE  "END"   TO   RD-FLG
         GO  TO  DSP-DOWN-990
     END-IF.
**
     IF  LINK-TORICD  =  ZERO
         MOVE   STB-F01     TO   WK-TORICD
     END-IF.
     MOVE     WK-TORICD     TO  STB-F01.
     MOVE     TBL-HSHOCD(MAX-LNCNT)    TO  STB-F02.
     START    HSHOTBL    KEY  IS  >  STB-F01  STB-F02
          INVALID
              MOVE  "END"    TO  RD-FLG
              MOVE   3       TO  ERR-FLG
              GO  TO    DSP-DOWN-990
     END-START.
     GO  TO   DSP-DOWN-040.
 DSP-DOWN-030.
*商品マスタ・カナ名位置付け
     IF  TBL-SEQ(MAX-LNCNT) =  ZERO  AND
         TBL-SHOK1(MAX-LNCNT) = SPACE
             MOVE     3    TO  ERR-FLG
             MOVE  "END"   TO   RD-FLG
         GO  TO  DSP-DOWN-990
     END-IF.
     MOVE     TBL-SHOK1(MAX-LNCNT)    TO  MKN-F031.
     MOVE     TBL-SHOK2(MAX-LNCNT)    TO  MKN-F032.
     START    HMKNMS    KEY  IS  >  MKN-F031  MKN-F032
          INVALID
              MOVE  "END"    TO  RD-FLG
              MOVE   3       TO  ERR-FLG
              GO  TO   DSP-DOWN-990
     END-START.
     GO  TO   DSP-DOWN-040.
 DSP-DOWN-040.
*マスタを読込みワークへセット
     PERFORM VARYING Y FROM 1 BY 1  UNTIL  Y  >  MAX-LNCNT
                                   OR RD-FLG = "END"
             PERFORM  MST-READ-SEC
             IF  RD-FLG NOT = "END"
                      MOVE  MEI-F011   TO  WK-DSP1-SSHO1
                      MOVE  MEI-F0121       TO  WK-DSP1-SSHO21
                      MOVE  MEI-F0122       TO  WK-DSP1-SSHO22
                      MOVE  MEI-F0123       TO  WK-DSP1-SSHO23
                      MOVE  "-"             TO  WK-DSP1-01
                            WK-DSP1-02  WK-DSP1-03
                      MOVE  WK-DSP1-SHOCD   TO  TBL-SHOCD(Y)
                      MOVE  MEI-F021        TO  WK-DSP1-SHON01
                      MOVE  MEI-F022        TO  WK-DSP1-SHON02
                      MOVE  WK-DSP1-SHON    TO  TBL-SHON(Y)
                      MOVE  STB-F02         TO  TBL-HSHOCD(Y)
                      MOVE  STB-F08         TO  TBL-TANABN(Y)
                      MOVE  MEI-F031        TO  TBL-SHOK1(Y)
                      MOVE  MEI-F032        TO  TBL-SHOK2(Y)
                      ADD   1   TO   WK-SEQ
                      MOVE  WK-SEQ          TO  TBL-SEQ(Y)
             END-IF
     END-PERFORM.
** テーブルエリア残クリア
     IF  WK-SEQ  =  0
         MOVE    3   TO  ERR-FLG
     ELSE
         IF   WK-SEQ   <  MAX-LNCNT
           ADD     1   TO  WK-SEQ
           PERFORM  UNTIL  WK-SEQ  >  MAX-LNCNT
              INITIALIZE      TABLE2(WK-SEQ)
              ADD   1    TO   WK-SEQ
           END-PERFORM
         END-IF
     END-IF.
**
 DSP-DOWN-990.
     IF  ERR-FLG  =  ZERO
         MOVE  "SET"        TO   SET-FLG
         PERFORM  WORK-DSP-SEC
         MOVE  SPACE        TO   SET-FLG
     END-IF.
**
 DSP-DOWN-EXIT.
     EXIT.
****************************************************************
*    前頁処理(PF11)                                     3.2    *
****************************************************************
 DSP-UP-SEC          SECTION.
     MOVE     "DSP-UP-SEC"   TO   S-NAME.
*ＳＥＱ番号セット
     MOVE      ZERO     TO    WK-SEQ.
     MOVE      SPACE    TO    RD-FLG.
*
     IF  WK-HYOJI  =  1
         GO  TO  DSP-UP-000
     END-IF.
     IF  WK-HYOJI  =  2
         GO  TO  DSP-UP-020
     END-IF.
     IF  WK-HYOJI  =  3
         GO  TO  DSP-UP-030
     END-IF.
     GO TO  DSP-UP-990.
 DSP-UP-000.
     IF  LINK-TORICD  NOT = ZERO
         GO  TO  DSP-UP-010
     END-IF.
*商品マスタ位置付け：自社コード
     IF  TBL-SEQ(01)        =  ZERO  AND
         TBL-SHOCD(01)      = SPACE
             MOVE   2       TO   ERR-FLG
             MOVE  "END"    TO   RD-FLG
         GO  TO  DSP-UP-990
     END-IF.
     MOVE     TBL-SHOCD(01)     TO  WK-DSP1-SHOCD.
     MOVE     WK-DSP1-SSHO1     TO  MEI-F011.
     MOVE     WK-DSP1-SSHO21    TO  MEI-F0121.
     MOVE     WK-DSP1-SSHO22    TO  MEI-F0122.
     MOVE     WK-DSP1-SSHO23    TO  MEI-F0123.
     START    HMEIMS    KEY  IS  <  MEI-F01
                        WITH  REVERSED
          INVALID
              MOVE  "END"   TO  RD-FLG
              MOVE   2      TO  ERR-FLG
              GO  TO  DSP-UP-990
     END-START.
     GO  TO   DSP-UP-040.
**
 DSP-UP-010.
*商品変換テーブル位置付け：自社コード
     IF  TBL-SEQ(01)        =  ZERO  AND
         TBL-SHOCD(01)      = SPACE
             MOVE   2       TO   ERR-FLG
             MOVE  "END"    TO   RD-FLG
         GO  TO  DSP-UP-990
     END-IF.
     MOVE     WK-TORICD         TO  SHT-F01.
     MOVE     TBL-SHOCD(01)     TO  WK-DSP1-SHOCD.
     MOVE     WK-DSP1-SSHO1     TO  SHT-F031.
     MOVE     WK-DSP1-SSHO21    TO  SHT-F032(1:5).
     MOVE     WK-DSP1-SSHO22    TO  SHT-F032(6:2).
     MOVE     WK-DSP1-SSHO23    TO  SHT-F032(8:1).
     START    SHOTBL4   KEY  IS  <  SHT-F01 SHT-F031 SHT-F032
                        WITH  REVERSED
          INVALID
              MOVE  "END"   TO  RD-FLG
              MOVE   2      TO  ERR-FLG
              GO  TO  DSP-UP-990
     END-START.
     GO  TO   DSP-UP-040.
**
 DSP-UP-020.
*商品変換テーブル位置付け：相手先コード
     IF  TBL-SEQ(01)   =  ZERO  AND
         TBL-HSHOCD(01)  = SPACE
             MOVE     2    TO  ERR-FLG
             MOVE  "END"   TO   RD-FLG
         GO  TO  DSP-UP-990
     END-IF.
**
     IF  LINK-TORICD  =  ZERO
         MOVE   STB-F01     TO   WK-TORICD
     END-IF.
     MOVE     WK-TORICD       TO  STB-F01.
     MOVE     TBL-HSHOCD(01)  TO  STB-F02.
     START    HSHOTBL    KEY  IS  <  STB-F01  STB-F02
                        WITH  REVERSED
          INVALID
              MOVE  "END"    TO  RD-FLG
              MOVE   2       TO  ERR-FLG
              GO  TO    DSP-UP-990
     END-START.
     GO  TO   DSP-UP-040.
 DSP-UP-030.
*商品マスタ・カナ名位置付け
     IF  TBL-SEQ(01) =  ZERO  AND
         TBL-SHOK1(01) = SPACE
             MOVE     2    TO  ERR-FLG
             MOVE  "END"   TO   RD-FLG
         GO  TO  DSP-UP-990
     END-IF.
     MOVE     TBL-SHOK1(01)    TO  MKN-F031.
     MOVE     TBL-SHOK2(01)    TO  MKN-F032.
     START    HMKNMS    KEY  IS  <   MKN-F031  MKN-F032
                        WITH  REVERSED
          INVALID
              MOVE  "END"    TO  RD-FLG
              MOVE   2       TO  ERR-FLG
              GO  TO    DSP-UP-990
     END-START.
     GO  TO   DSP-UP-040.
 DSP-UP-040.
     COMPUTE  WK-SEQ  =  MAX-LNCNT +  1.
*マスタを読込みワークへセット
     PERFORM VARYING Y FROM 1 BY 1  UNTIL  Y  >  MAX-LNCNT
                                   OR RD-FLG = "END"
       COMPUTE  X  =  ( MAX-LNCNT  +  1  )  -  Y
       END-COMPUTE
             PERFORM  MST-READ-SEC
             IF  RD-FLG NOT = "END"
                      MOVE  MEI-F011        TO  WK-DSP1-SSHO1
                      MOVE  MEI-F0121       TO  WK-DSP1-SSHO21
                      MOVE  MEI-F0122       TO  WK-DSP1-SSHO22
                      MOVE  MEI-F0123       TO  WK-DSP1-SSHO23
                      MOVE  "-"             TO  WK-DSP1-01
                            WK-DSP1-02  WK-DSP1-03
                      MOVE  WK-DSP1-SHOCD   TO  TBL-SHOCD(X)
                      MOVE  MEI-F021        TO  WK-DSP1-SHON01
                      MOVE  MEI-F022        TO  WK-DSP1-SHON02
                      MOVE  WK-DSP1-SHON    TO  TBL-SHON(X)
                      MOVE  STB-F02         TO  TBL-HSHOCD(X)
                      MOVE  STB-F08         TO  TBL-TANABN(X)
                      MOVE  MEI-F031        TO  TBL-SHOK1(X)
                      MOVE  MEI-F032        TO  TBL-SHOK2(X)
                      SUBTRACT  1  FROM  WK-SEQ
                      MOVE  WK-SEQ          TO  TBL-SEQ(X)
             END-IF
     END-PERFORM.
** テーブルエリア残クリア
     IF  WK-SEQ  >  MAX-LNCNT
         MOVE    2   TO  ERR-FLG
     ELSE
         IF  WK-SEQ  NOT  =  1
             SUBTRACT  1  FROM  WK-SEQ
             PERFORM  UNTIL  WK-SEQ  =  0
                INITIALIZE      TABLE2(WK-SEQ)
                SUBTRACT  1  FROM  WK-SEQ
             END-PERFORM
         END-IF
     END-IF.
 DSP-UP-990.
     IF  ERR-FLG  =  ZERO
         MOVE  "SET"        TO   SET-FLG
         PERFORM  WORK-DSP-SEC
         MOVE  SPACE        TO   SET-FLG
     END-IF.
**
 DSP-UP-EXIT.
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
         WHEN   "1"
           MOVE    PF-MSG-R(1)        TO   DSP-GUIDE
         WHEN   "2"
           MOVE    PF-MSG-R(2)        TO   DSP-GUIDE
         WHEN   "3"
           MOVE    PF-MSG-R(3)        TO   DSP-GUIDE
     END-EVALUATE.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FSUB0301"          TO   DSP-FMT.
     WRITE    DSP-FSUB0301.
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
*検索順
         WHEN   "1"
                MOVE    "HEAD"    TO   DSP-GRP
*選択
         WHEN   "2"
                MOVE    "SENTAK"  TO   DSP-GRP
*確認
         WHEN   "3"
                MOVE    "KAKU"    TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FSUB0301"           TO   DSP-FMT.
     READ    DSPFILE.
*入力項目の属性を通常にする
 DSP-READ-010.
*    MOVE    ZERO                 TO   ERR-FLG.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             画面制御項目初期化                               *
****************************************************************
 DSP-SYOKI-SEC         SECTION.
     MOVE     "DSP-SYOKI-SEC"    TO   S-NAME.
*リバース，カーソルパーク解除
***  検索順
     MOVE "M"   TO EDIT-OPTION OF DSP-HYOJI.
     MOVE SPACE TO EDIT-CURSOR OF DSP-HYOJI.
***  サカタ商品ＣＤ
     MOVE "M"   TO EDIT-OPTION OF DSP-SSHO1.
     MOVE SPACE TO EDIT-CURSOR OF DSP-SSHO1.
     MOVE "M"   TO EDIT-OPTION OF DSP-SSHO2.
     MOVE SPACE TO EDIT-CURSOR OF DSP-SSHO2.
     MOVE "M"   TO EDIT-OPTION OF DSP-SSHO3.
     MOVE SPACE TO EDIT-CURSOR OF DSP-SSHO3.
     MOVE "M"   TO EDIT-OPTION OF DSP-SSHO4.
     MOVE SPACE TO EDIT-CURSOR OF DSP-SSHO4.
***  相手商品ＣＤ
     MOVE "M"   TO EDIT-OPTION OF DSP-HSHO.
     MOVE SPACE TO EDIT-CURSOR OF DSP-HSHO.
***  カナ名ＣＤ
     MOVE "M"   TO EDIT-OPTION OF DSP-SHOKN.
     MOVE SPACE TO EDIT-CURSOR OF DSP-SHOKN.
***  選択番号
     MOVE "M"   TO EDIT-OPTION OF DSP-SENNO.
     MOVE SPACE TO EDIT-CURSOR OF DSP-SENNO.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*           各マスタ読込み                                     *
****************************************************************
 MST-READ-SEC          SECTION.
     MOVE     "MST-READ-SEC"   TO   S-NAME.
*
     IF  WK-HYOJI  =  1
         GO  TO  MST-READ-000
     END-IF.
     IF  WK-HYOJI  =  2
         GO  TO  MST-READ-020
     END-IF.
     IF  WK-HYOJI  =  3
         GO  TO  MST-READ-030
     END-IF.
     GO TO  MST-READ-990.
 MST-READ-000.
     IF  LINK-TORICD  NOT = ZERO
         GO  TO  MST-READ-010
     END-IF.
*商品マスタ：自社コード
     PERFORM  HMEIMS-READ-NEXT
     IF  RD-FLG  =  SPACE
         MOVE     WK-TORICD     TO  SHT-F01
         MOVE     MEI-F011      TO  SHT-F031
         MOVE     MEI-F012      TO  SHT-F032
** 商品変換テーブル（乱）：自社コード
         PERFORM  HSHOTB4-R-SEC
*
         MOVE     SHT-REC       TO  STB-REC
     END-IF.
     GO  TO   MST-READ-040.
**
 MST-READ-010.
*商品変換テーブル：自社コード
     PERFORM  HSHOTB4-READ-NEXT
     IF  RD-FLG  =  SPACE
         MOVE     SHT-F031      TO  MEI-F011
         MOVE     SHT-F032      TO  MEI-F012
** 商品マスタ（乱）**
         PERFORM  HMEIMS-R-SEC
*
***##    IF  INV-FLG  =  "INV"
***##        GO  TO    MST-READ-010
***##    END-IF
         MOVE     SHT-REC       TO  STB-REC
     END-IF.
     GO  TO   MST-READ-040.
**
 MST-READ-020.
*商品変換テーブル：相手先コード
     PERFORM  HSHOTBL-READ-NEXT.
     IF  RD-FLG  =  SPACE
         MOVE     STB-F031      TO  MEI-F011
         MOVE     STB-F032      TO  MEI-F012
** 商品マスタ（乱）**
         PERFORM  HMEIMS-R-SEC
     END-IF.
     GO  TO   MST-READ-040.
 MST-READ-030.
*商品マスタ・カナ名
     PERFORM  HMKNMS-READ-NEXT.
     IF  RD-FLG  =  SPACE
         MOVE     MKN-REC       TO  MEI-REC
         MOVE     WK-TORICD     TO  SHT-F01
         MOVE     MEI-F011      TO  SHT-F031
         MOVE     MEI-F012      TO  SHT-F032
** 商品変換テーブル（乱）：自社コード
         PERFORM  HSHOTB4-R-SEC
         MOVE     SHT-REC       TO  STB-REC
         IF  INV-FLG  =  "INV"
           AND  LINK-TORICD  NOT =  ZERO
             GO  TO    MST-READ-030
         END-IF
     END-IF.
     GO  TO   MST-READ-040.
**
 MST-READ-040.
**
 MST-READ-990.
**
 MST-READ-EXIT.
     EXIT.
****************************************************************
*             商品名称マスタ読込み                   3.2.1     *
****************************************************************
 HMEIMS-READ-NEXT      SECTION.
*
     MOVE     "HMEIMS-READ-NEXT"   TO   S-NAME.
     MOVE     SPACE            TO   RD-FLG.
*マスタ読込み
     READ   HMEIMS  NEXT  AT END
          MOVE    "END"           TO   RD-FLG
          GO  TO             HMEIMS-READ-NEXT-EXIT
     END-READ.
*
 HMEIMS-READ-NEXT-EXIT.
     EXIT.
****************************************************************
*             商品名称マスタ・カナ読込み             3.2.2     *
****************************************************************
 HMKNMS-READ-NEXT      SECTION.
*
     MOVE     "HMKNMS-READ-NEXT"   TO   S-NAME.
     MOVE     SPACE            TO   RD-FLG.
*マスタ読込み
     READ   HMKNMS  NEXT  AT END
          MOVE    "END"           TO   RD-FLG
          GO  TO             HMKNMS-READ-NEXT-EXIT
     END-READ.
*
 HMKNMS-READ-NEXT-EXIT.
     EXIT.
****************************************************************
*             商品変換テーブル読込み                 3.2.3     *
****************************************************************
 HSHOTBL-READ-NEXT      SECTION.
*
     MOVE     "HSHOTBL-READ-NEXT"   TO   S-NAME.
     MOVE     SPACE            TO   RD-FLG.
*マスタ読込み
     READ   HSHOTBL  NEXT  AT END
          MOVE    "END"           TO   RD-FLG
          GO  TO             HSHOTBL-READ-NEXT-EXIT
     END-READ.
*
*** 指定取引先以外は対象外
     IF  STB-F01  NOT = WK-TORICD
       AND  LINK-TORICD  NOT = ZERO
         MOVE  "END"        TO   RD-FLG
     END-IF.
 HSHOTBL-READ-NEXT-EXIT.
     EXIT.
****************************************************************
*             商品変換テーブル４読込み               3.2.3     *
****************************************************************
 HSHOTB4-READ-NEXT      SECTION.
*
     MOVE     "HSHOTB4-READ-NEXT"   TO   S-NAME.
     MOVE     SPACE            TO   RD-FLG.
*マスタ読込み
     READ   SHOTBL4  NEXT  AT END
          MOVE    "END"           TO   RD-FLG
          GO  TO             HSHOTB4-READ-NEXT-EXIT
     END-READ.
*
*** 指定取引先以外は対象外
     IF  SHT-F01  NOT = WK-TORICD
         MOVE  "END"        TO   RD-FLG
     END-IF.
 HSHOTB4-READ-NEXT-EXIT.
     EXIT.
****************************************************************
*             商品名称マスタ読込み（乱）             3.3.1     *
****************************************************************
 HMEIMS-R-SEC          SECTION.
*
     MOVE     "HMEIMS-R-SEC"   TO   S-NAME.
     MOVE     SPACE            TO   INV-FLG.
*マスタ読込み
     READ   HMEIMS  INVALID
          MOVE    SPACE           TO   MEI-REC
          INITIALIZE                   MEI-REC
          MOVE    "INV"           TO   INV-FLG
          GO  TO             HMEIMS-R-EXIT
     END-READ.
*
 HMEIMS-R-EXIT.
     EXIT.
****************************************************************
*             商品変換テーブル読込み（乱）           3.3.2     *
****************************************************************
 HSHOTBL-R-SEC          SECTION.
*
     MOVE     "HSHOTBL-R-SEC"   TO   S-NAME.
     MOVE     SPACE            TO   INV-FLG.
*マスタ読込み
     READ   HSHOTBL  INVALID
          MOVE    SPACE           TO   STB-REC
          INITIALIZE                   STB-REC
          MOVE    "INV"           TO   INV-FLG
          GO  TO             HSHOTBL-R-EXIT
     END-READ.
*
 HSHOTBL-R-EXIT.
     EXIT.
****************************************************************
*             商品変換テーブル読込み４（乱）         3.3.3     *
****************************************************************
 HSHOTB4-R-SEC          SECTION.
*
     MOVE     "HSHOTB4-R-SEC"   TO   S-NAME.
     MOVE     SPACE            TO   INV-FLG.
*マスタ読込み
     READ   SHOTBL4  INVALID
          MOVE    SPACE           TO   SHT-REC
          INITIALIZE                   SHT-REC
          MOVE    "INV"           TO   INV-FLG
          GO  TO             HSHOTB4-R-EXIT
     END-READ.
*
 HSHOTB4-R-EXIT.
     EXIT.
*******************< PROGRAM-END FSB0301I >*********************

```

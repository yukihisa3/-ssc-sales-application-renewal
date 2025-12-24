****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　共通システム　　　　　　　　　　　*
*    業務名　　　　　　　：　　　　　　　　　　　　　　        *
*    モジュール名　　　　：　量販店商品コード照会              *
*    作成日／作成者　　　：　1999/10/08  /HAGIWARA             *
*    更新日／更新者　　　：　　　　　　                        *
*    処理概要　　　　　　：　サカタ商品コードから，量販店商品　*
*                            コードを一覧照会する。            *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SKY1202I.
 AUTHOR.               HAGIWARA.
 DATE-WRITTEN.         99/10/08.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*画面ファイル
*商品変換テーブル
     SELECT  HSHOTBL   ASSIGN    TO        SHOTBL6
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      DYNAMIC
                       RECORD    KEY       SHO-F03
                                           SHO-F01
                       FILE      STATUS    SHO-ST.
*商品名称マスタ　
     SELECT  HMEIMS    ASSIGN    TO        MEIMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       MEI-F011
                                           MEI-F012
                       FILE      STATUS    MEI-ST.
*取引先マスタ
     SELECT  HTOKMS    ASSIGN    TO        TOKMS2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TOK-F01
                       FILE      STATUS    TOK-ST.
*条件ファイル
     SELECT  HJYOKEN   ASSIGN    TO        JYOKEN1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       JYO-F01
                                           JYO-F02
                       FILE      STATUS    JYO-ST.
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
*    FILE = 商品変換テーブル                                   *
****************************************************************
 FD  HSHOTBL
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HSHOTBL   OF   XFDLIB
                       JOINING   SHO       AS   PREFIX.
****************************************************************
*    FILE = 商品名称マスタ　                                   *
****************************************************************
 FD  HMEIMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      MEIMS1    OF   XFDLIB
                       JOINING   MEI       AS   PREFIX.
****************************************************************
*    FILE = 取引先マスタ                                       *
****************************************************************
 FD  HTOKMS
                       BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*    FILE = 条件ファイル                                       *
****************************************************************
 FD  HJYOKEN
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HJYOKEN   OF   XFDLIB
                       JOINING   JYO       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FKY12011  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  SHO-ST                   PIC  X(02).
     03  MEI-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  JYO-ST                   PIC  X(02).
     03  DSP-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
*フラグ領域
 01  FLG-AREA.
     03  HTOKMS-INV-FLG           PIC  X(03)  VALUE  SPACE.
     03  CHK-FLG                  PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG                  PIC  9(01)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  RD-FLG                   PIC  X(03)  VALUE  SPACE.
     03  SET-FLG                  PIC  X(03)  VALUE  SPACE.
     03  P-CNT                    PIC  9(01)  VALUE  ZERO.
     03  S-CNT                    PIC  9(01)  VALUE  ZERO.
     03  C-CNT                    PIC  9(01)  VALUE  ZERO.
     03  X                        PIC  9(02)  VALUE  ZERO.
     03  Y                        PIC  9(02)  VALUE  ZERO.
     03  IX                       PIC  9(01)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
***  ＳＥＱ_用ワーク
     03  WK-NO                    PIC  9(02)  VALUE  ZERO.
***  商品変換テーブルSHO-F03用ワーク
 01  SHOTBL-KEY.
     03  KEY-01                   PIC  X(08)  VALUE  SPACE.
     03  KEY-02                   PIC  X(05)  VALUE  SPACE.
     03  KEY-03                   PIC  X(02)  VALUE  SPACE.
     03  KEY-04                   PIC  X(01)  VALUE  SPACE.
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
*特販部名称編集
 01  HEN-TOKHAN-AREA.
     03  FILLER                   PIC  N(01)  VALUE  NC"（".
     03  HEN-TOKHAN               PIC  N(06)  VALUE  SPACE.
     03  FILLER                   PIC  N(01)  VALUE  NC"）".
*
*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-MSG1.
         05  FILLER               PIC   N(20)
      VALUE NC"_取消_終了　　　　　　　　　　　".
     03  PF-MSG2.
         05  FILLER               PIC   N(20)
      VALUE NC"_取消_終了_前頁_次頁".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R   OCCURS   2   PIC   N(20).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(20)
             VALUE NC"コードを正しく入力して下さい。".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"無効キーです。".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"前頁はありません。　　　".
     03  ERR-MSG4.
         05  FILLER              PIC   N(20)
             VALUE NC"次頁はありません。　　　".
     03  ERR-MSG5.
         05  FILLER              PIC   N(20)
             VALUE NC"入力されたコードは存在しません。".
     03  ERR-MSG6.
         05  FILLER              PIC   N(20)
             VALUE NC"入力されたコードの名称が存在しません。".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  6   PIC   N(20).
*商品変換テーブル退避エリア
 01  TABLE-AREA.
     03  TBL-SKTCD1               PIC   X(08).
     03  TBL-SKTCD2               PIC   X(05).
     03  TBL-SKTCD3               PIC   X(02).
     03  TBL-SKTCD4               PIC   X(01).
*
     03  TABLE1      OCCURS  6.
         05  TABLE2  OCCURS  12.
             07  TBL-NO           PIC   9(02).
             07  TBL-RYOCD        PIC   9(08).
             07  TBL-RYONM        PIC   N(05).
             07  TBL-SHOCD        PIC   X(13).
             07  TBL-GENKA        PIC   9(07)V9(02).
             07  TBL-BAIKA        PIC   9(07)V9(02).
             07  TBL-TANANO       PIC   X(06).
             07  TBL-SOKOCD       PIC   X(02).
*
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  SHO-ERR           PIC N(15) VALUE
                        NC"商品変換テーブルエラー".
     03  MEI-ERR           PIC N(15) VALUE
                        NC"商品名称マスタエラー".
     03  TOK-ERR           PIC N(15) VALUE
                        NC"取引先マスタエラー".
     03  JYO-ERR           PIC N(15) VALUE
                        NC"条件ファイルエラー".
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
 01  WK-SHOCD.
     03  WK-SHO            PIC  X(01)    OCCURS  8.
 01  WK-HINCD.
     03  WK-HIN            PIC  X(01)    OCCURS  5.
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
 SHO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HSHOTBL.
     MOVE        SHO-ST    TO        E-ST.
     MOVE        "HSHOTBL" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SHO-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HMEIMS.
     MOVE        MEI-ST    TO        E-ST.
     MOVE        "HMEIMS"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SHO-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     MOVE        TOK-ST    TO        E-ST.
     MOVE        "HTOKMS"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TOK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HJYOKEN.
     MOVE        JYO-ST    TO        E-ST.
     MOVE        "HJYOKEN" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JYO-ERR   UPON      CONS.
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
*ファイルのＯＰＥＮ
     OPEN      INPUT    HJYOKEN   HSHOTBL   HMEIMS    HTOKMS.
     OPEN      I-O                DSPFILE.
*ワークの初期化
     INITIALIZE                 FLG-AREA.
*
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
*特販部名称編集
     MOVE     SPACE               TO   JYO-REC.
     INITIALIZE                        JYO-REC.
     MOVE    "99"                 TO   JYO-F01.
     MOVE    "BUMON"              TO   JYO-F02.
     READ     HJYOKEN
       INVALID KEY
              MOVE  NC"＊＊＊＊＊＊"   TO   HEN-TOKHAN
       NOT INVALID KEY
              MOVE  JYO-F03            TO   HEN-TOKHAN
     END-READ.
*
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
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
*初期画面表示(2.1)
         WHEN      "1"       PERFORM   DSP-INIT-SEC
*ヘッダ部入力(2.2)
         WHEN      "2"       PERFORM   DSP-HEAD-SEC
*確認入力    (2.3)
         WHEN      "3"       PERFORM   DSP-KAKU-SEC
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
*ファイル ＣＬＯＳＥ
     CLOSE    HJYOKEN  DSPFILE   HSHOTBL   HMEIMS   HTOKMS.
**
 END-EXIT.
     EXIT.
****************************************************************
*             初期画面表示( PSW = 1 )                2.1       *
****************************************************************
 DSP-INIT-SEC          SECTION.
     MOVE     "DSP-INIT-SEC"      TO   S-NAME.
*初期画面の表示
     MOVE     SPACE               TO   DSP-PRO.
*画面の初期化
     MOVE    SPACE                TO   DSP-FKY12011.
*システム日付転送
     MOVE    HEN-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    HEN-TIME             TO   DSP-STIME.
     MOVE    HEN-TOKHAN-AREA      TO   DSP-TOKHAN.
*項目属性クリア
     PERFORM  DSP-SYOKI-SEC.
*ヘッダ部入力へ
     IF       SET-FLG  =  "SET"
              MOVE    "3"         TO   PSW
     ELSE
              MOVE    "2"         TO   PSW
     END-IF.
*
 DSP-INIT-EXIT.
     EXIT.
****************************************************************
*             ヘッダ部入力( PSW = 2 )                2.2       *
****************************************************************
 DSP-HEAD-SEC          SECTION.
     MOVE     "DSP-HEAD-SEC"      TO   S-NAME.
*
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"
                PERFORM   HEAD-CHK-SEC
                IF        ERR-FLG  =  ZERO
                          MOVE  "3"   TO   PSW
                          MOVE   1    TO   P-CNT
                          PERFORM     WORK-DSP-SEC
                END-IF
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
         WHEN   OTHER
                MOVE     2       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-HEAD-EXIT.
     EXIT.
****************************************************************
*             ヘッダ部チェック                 2.2.1           *
****************************************************************
 HEAD-CHK-SEC             SECTION.
     MOVE     "HEAD-CHK-SEC"     TO   S-NAME.
*サカタコード未入力チェック
     IF       DSP-SKTCD1     =   SPACE
              MOVE   1       TO  ERR-FLG
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SKTCD1
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SKTCD2
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SKTCD3
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SKTCD4
              MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-SKTCD1
              GO     TO      HEAD-CHK-EXIT
     ELSE
**************商品ＣＤ桁埋め
              MOVE   DSP-SKTCD1   TO  WK-SHOCD
              PERFORM          UNTIL  WK-SHO(8) NOT = SPACE
                 PERFORM VARYING IX FROM 7 BY -1 UNTIL IX = 0
                    MOVE   WK-SHO(IX)       TO  WK-SHO(IX + 1)
                 END-PERFORM
                 MOVE      ZERO             TO  WK-SHO(1)
              END-PERFORM
              MOVE  WK-SHOCD TO  DSP-SKTCD1
              IF    DSP-SKTCD2 NOT = SPACE
                    MOVE   DSP-SKTCD2   TO  WK-HINCD
                    PERFORM          UNTIL  WK-HIN(5) NOT = SPACE
                      PERFORM VARYING IX FROM 4 BY -1 UNTIL IX = 0
                        MOVE   WK-HIN(IX)       TO  WK-HIN(IX + 1)
                      END-PERFORM
                    MOVE      SPACE            TO  WK-HIN(1)
                    END-PERFORM
                    MOVE  WK-HINCD TO  DSP-SKTCD2
              END-IF
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SKTCD1
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SKTCD2
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SKTCD3
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SKTCD4
              MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-SKTCD1
     END-IF.
*サカタコード存在チェック
     PERFORM  HSHOTBL-START-SEC.
     IF       CHK-FLG  =  "CHK"
              MOVE      5      TO   ERR-FLG
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SKTCD1
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SKTCD2
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SKTCD3
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SKTCD4
              MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-SKTCD1
              GO     TO      HEAD-CHK-EXIT
       ELSE
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SKTCD1
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SKTCD2
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SKTCD3
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SKTCD4
              MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-SKTCD1
       END-IF.
*マスタからワークへデータセット
     IF       ERR-FLG  =  ZERO
              PERFORM     MST-WORK-SEC
     END-IF.
*
 HEAD-CHK-EXIT.
     EXIT.
****************************************************************
*             マスタ→ワーク（退避）                           *
****************************************************************
 MST-WORK-SEC          SECTION.
     MOVE     "MST-WORK-SEC"   TO   S-NAME.
*ワークテーブルクリア
     MOVE     SPACE            TO   TABLE-AREA.
     INITIALIZE                     TABLE-AREA.
     MOVE     ZERO             TO   WK-NO.
*ヘッダ部のセット
***  サカタコード
     MOVE     DSP-SKTCD1       TO   TBL-SKTCD1.
     MOVE     DSP-SKTCD2       TO   TBL-SKTCD2.
     MOVE     DSP-SKTCD3       TO   TBL-SKTCD3.
     MOVE     DSP-SKTCD4       TO   TBL-SKTCD4.
*商品変換テーブル読込み
     PERFORM  HSHOTBL-READ-SEC.
     IF       RD-FLG  =  "END"
              MOVE   5       TO  ERR-FLG
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SKTCD1
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SKTCD2
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SKTCD3
              MOVE  "R"      TO  EDIT-OPTION  OF  DSP-SKTCD4
              MOVE  "C"      TO  EDIT-CURSOR  OF  DSP-SKTCD1
              GO             TO  MST-WORK-EXIT
     ELSE
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SKTCD1
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SKTCD2
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SKTCD3
              MOVE  "M"      TO  EDIT-OPTION  OF  DSP-SKTCD4
              MOVE   SPACE   TO  EDIT-CURSOR  OF  DSP-SKTCD1
     END-IF.
*商品変換テーブルを読込みながらワークへセット
     PERFORM VARYING X FROM 1 BY 1 UNTIL X > 6
                                   OR SHOTBL-KEY NOT = SHO-F03
                                   OR RD-FLG = "END"
             PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 12
                                   OR SHOTBL-KEY NOT = SHO-F03
                                   OR RD-FLG = "END"
                     ADD     1    TO WK-NO
                     MOVE WK-NO   TO TBL-NO(X Y)
                     MOVE SHO-F01 TO TBL-RYOCD(X Y)
                     MOVE SHO-F02 TO TBL-SHOCD(X Y)
                     MOVE SHO-F05 TO TBL-GENKA(X Y)
                     MOVE SHO-F06 TO TBL-BAIKA(X Y)
                     MOVE SHO-F08 TO TBL-TANANO(X Y)
                     MOVE SHO-F04 TO TBL-SOKOCD(X Y)
                     PERFORM  HSHOTBL-READ-SEC
             END-PERFORM
     END-PERFORM.
     CLOSE  HSHOTBL.
     OPEN   INPUT HSHOTBL.
**
 MST-WORK-EXIT.
     EXIT.
****************************************************************
*             ワーク→画面表示                                 *
****************************************************************
 WORK-DSP-SEC          SECTION.
     MOVE     "WORK-DSP-SEC"     TO   S-NAME.
*項目画面セット
     MOVE     TBL-SKTCD1         TO   DSP-SKTCD1  MEI-F011.
     MOVE     TBL-SKTCD2         TO   DSP-SKTCD2  MEI-F0121.
     MOVE     TBL-SKTCD3         TO   DSP-SKTCD3  MEI-F0122.
     MOVE     TBL-SKTCD4         TO   DSP-SKTCD4  MEI-F0123.
***  商品名称取得
     READ     HMEIMS
       INVALID
*             MOVE          6         TO   ERR-FLG
*             MOVE          SPACE     TO   DSP-SHONM1
*             MOVE          SPACE     TO   DSP-SHONM2
*             MOVE          SPACE     TO   DSP-SHOKN1
*             MOVE          SPACE     TO   DSP-SHOKN2
*             MOVE          SPACE     TO   DSP-JANCD
*             GO            TO        WORK-DSP-SEC
              MOVE NC"名称マスタ未登録"    TO   DSP-SHONM1
              MOVE NC"名称マスタ未登録"    TO   DSP-SHONM2
       NOT INVALID
              MOVE          MEI-F021  TO   DSP-SHONM1
              MOVE          MEI-F022  TO   DSP-SHONM2
              MOVE          MEI-F031  TO   DSP-SHOKN1
              MOVE          MEI-F032  TO   DSP-SHOKN2
              MOVE          MEI-F06   TO   DSP-JANCD
     END-READ.
***
     PERFORM VARYING Y FROM 1 BY 1 UNTIL Y > 12
         IF  TBL-NO(P-CNT Y)  NOT =  ZERO
             MOVE    TBL-NO(P-CNT Y)     TO DSP-NO(Y)
             MOVE    TBL-RYOCD(P-CNT Y)  TO DSP-RYOCD(Y) TOK-F01
***          量販店名称取得
             READ    HTOKMS
               INVALID
                     MOVE    SPACE       TO   DSP-RYONM(Y)
               NOT INVALID
                     MOVE    TOK-F03     TO   DSP-RYONM(Y)
             END-READ
***
             MOVE    TBL-SHOCD(P-CNT Y)  TO DSP-SHOCD(Y)
             MOVE    TBL-GENKA(P-CNT Y)  TO DSP-GENKA(Y)
             MOVE    TBL-BAIKA(P-CNT Y)  TO DSP-BAIKA(Y)
             MOVE    TBL-TANANO(P-CNT Y) TO DSP-TANANO(Y)
             MOVE    TBL-SOKOCD(P-CNT Y) TO DSP-SOKOCD(Y)
         END-IF
     END-PERFORM.
*
 WORK-DSP-EXIT.
     EXIT.
****************************************************************
*             確認処理　入力（ PSW = 3 ）            2.4
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
                MOVE    "3"      TO   PSW
*取消
         WHEN   "F004"
                MOVE    "1"      TO   PSW
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*前頁
         WHEN   "F011"
                COMPUTE C-CNT = P-CNT - 1
                IF      C-CNT = ZERO
                        MOVE  3  TO  ERR-FLG
                ELSE
                        COMPUTE P-CNT = P-CNT - 1
                        MOVE    "SET"   TO   SET-FLG
                        PERFORM DSP-INIT-SEC
                        PERFORM WORK-DSP-SEC
                        MOVE    SPACE   TO   SET-FLG
                END-IF
*次頁
         WHEN   "F012"
                MOVE    P-CNT   TO      S-CNT
                COMPUTE C-CNT = P-CNT + 1
                IF      C-CNT > 6
                        MOVE   4   TO   ERR-FLG
                ELSE
                  IF    TBL-NO(C-CNT 1)  =  ZERO
                        MOVE   4     TO    ERR-FLG
                  ELSE
                        COMPUTE P-CNT = P-CNT + 1
                        MOVE    "SET"   TO   SET-FLG
                        PERFORM DSP-INIT-SEC
                        PERFORM WORK-DSP-SEC
                        MOVE    SPACE   TO   SET-FLG
                  END-IF
                END-IF
         WHEN   OTHER
                MOVE     2       TO   ERR-FLG
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
*ヘッダ　　
         WHEN   "1"    WHEN    "2"
                MOVE    PF-MSG-R(1)        TO   DSP-PFGAID
*明細／確認
         WHEN   "3"
                MOVE    PF-MSG-R(2)        TO   DSP-PFGAID
     END-EVALUATE.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FKY12011"          TO   DSP-FMT.
     WRITE    DSP-FKY12011.
     PERFORM  DSP-SYOKI-SEC.
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
*曜日
         WHEN   "2"
                MOVE    "HEAD"    TO   DSP-GRP
*確認
         WHEN   "3"
                MOVE    "KAKU"    TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FKY12011"           TO   DSP-FMT.
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
***
     MOVE "M"   TO EDIT-OPTION OF DSP-SKTCD1.
     MOVE "M"   TO EDIT-OPTION OF DSP-SKTCD2.
     MOVE "M"   TO EDIT-OPTION OF DSP-SKTCD3.
     MOVE "M"   TO EDIT-OPTION OF DSP-SKTCD4.
     MOVE SPACE TO EDIT-CURSOR OF DSP-SKTCD1.
     MOVE SPACE TO EDIT-CURSOR OF DSP-SKTCD2.
     MOVE SPACE TO EDIT-CURSOR OF DSP-SKTCD3.
     MOVE SPACE TO EDIT-CURSOR OF DSP-SKTCD4.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             商品変換テーブルＳＴＡＲＴ                       *
****************************************************************
 HSHOTBL-START-SEC     SECTION.
*
     MOVE     "HSHOTBL-START-SEC" TO   S-NAME.
*サカタコードにより商品変換テーブルスタート
     MOVE       SPACE          TO     RD-FLG.
     MOVE       SPACE          TO     CHK-FLG.
     MOVE       DSP-SKTCD1     TO     KEY-01.
     MOVE       DSP-SKTCD2     TO     KEY-02.
     MOVE       DSP-SKTCD3     TO     KEY-03.
     MOVE       DSP-SKTCD4     TO     KEY-04.
     MOVE       SHOTBL-KEY     TO     SHO-F03.
     MOVE       ZERO           TO     SHO-F01.
     START      HSHOTBL  KEY  IS  >=  SHO-F03
                                      SHO-F01
        INVALID
                MOVE    "CHK"  TO     CHK-FLG
        NOT INVALID
                IF      SHO-F03 NOT = SHOTBL-KEY
                MOVE    "CHK"  TO     CHK-FLG
            END-IF
     END-START.
*
 HSHOTBL-START-EXIT.
     EXIT.
****************************************************************
*             商品変換テーブル読込み                           *
****************************************************************
 HSHOTBL-READ-SEC      SECTION.
     MOVE     "HSHOTBL-READ-SEC"  TO   S-NAME.
*
     READ HSHOTBL NEXT AT END
          MOVE   "END"    TO   RD-FLG
     END-READ.
*画面コード以外の時、終了へ
     IF      SHO-F031  NOT = DSP-SKTCD1
         OR  SHO-F0321 NOT = DSP-SKTCD2
         OR  SHO-F0322 NOT = DSP-SKTCD3
         OR  SHO-F0323 NOT = DSP-SKTCD4
             MOVE     "END"       TO   RD-FLG
     END-IF.
*
 HSHOTBL-READ-EXIT.
     EXIT.
*******************< PROGRAM-END SKY1202I >*********************

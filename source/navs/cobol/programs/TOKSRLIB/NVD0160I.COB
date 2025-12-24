****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　入荷予定表発行指示　　　　　　　　*
*    作成日／作成者　　　：　2020/02/25   ASS.II               *
*    処理内容　　　　　　：　入荷予定表を一覧照会する。　　　　*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           NVD0160I.
 AUTHOR.               ASS.II.
 DATE-WRITTEN.         20/02/20.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*画面定義ファイル
     SELECT  DSPFILE        ASSIGN    TO        GS-DSPF
                            ORGANIZATION         IS  SEQUENTIAL
                            ACCESS MODE          IS  SEQUENTIAL
                            SYMBOLIC DESTINATION IS "DSP"
                            FORMAT              DSP-FMT
                            GROUP               DSP-GRP
                            PROCESSING          DSP-PRO
                            FUNCTION            DSP-FNC
                            FILE      STATUS    DSP-ST.
*発注残照会ワーク
     SELECT   NZANWKF        ASSIGN        TO  01-VI-NZANWKL1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  SEQUENTIAL
                             RECORD KEY    IS  ZAN-F01
                                               ZAN-F02
                                               ZAN-F03
                             FILE STATUS   IS  ZAN-ST.
*発注Ｆ（ヘッダ）
     SELECT   HACHEDF        ASSIGN        TO  01-VI-HACHEDL2
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  SEQUENTIAL
                             RECORD KEY    IS  HED-F17
                                               HED-F02
                             FILE STATUS   IS  HED-ST.
*発注Ｆ（明細）
     SELECT   HACMEIF        ASSIGN        TO  01-VI-HACMEIL1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  SEQUENTIAL
                             RECORD KEY    IS  BDY-F02
                                               BDY-F03
                             FILE STATUS   IS  BDY-ST.
*仕入先Ｍ
     SELECT   ZSHIMS         ASSIGN        TO  01-VI-ZSHIMS1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  SHI-F01
                             FILE STATUS   IS  SHI-ST.

*倉庫Ｍ
     SELECT   ZSOKMS         ASSIGN        TO  01-VI-ZSOKMS1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  SOK-F01
                             FILE STATUS   IS  SOK-ST.
*商品名称Ｍ
     SELECT   HMEIMS         ASSIGN        TO  01-VI-MEIMS1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  DYNAMIC
                             RECORD KEY    IS  MEI-F011
                                               MEI-F012
                             FILE STATUS   IS  MEI-ST.
*---<<  条件ファイル  >>---*
     SELECT   HJYOKEN        ASSIGN        TO  DA-01-VI-JYOKEN1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  JYO-F01
                                               JYO-F02
                             FILE STATUS   IS  JYO-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE.
                       COPY      FVD01601  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
*    FILE = 発注残照会ワーク                                   *
****************************************************************
 FD  NZANWKF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NZANWKF   OF   XFDLIB
                       JOINING   ZAN       AS   PREFIX.
****************************************************************
*    FILE = 発注ファイル（ヘッダ）                             *
****************************************************************
 FD  HACHEDF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HACHEDF   OF   XFDLIB
                       JOINING   HED       AS   PREFIX.
****************************************************************
*    FILE = 発注ファイル（明細）                               *
****************************************************************
 FD  HACMEIF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HACMEIF   OF   XFDLIB
                       JOINING   BDY       AS   PREFIX.
****************************************************************
*    FILE = 仕入先マスタ　                                   *
****************************************************************
 FD  ZSHIMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ZSHIMS    OF   XFDLIB
                       JOINING   SHI       AS   PREFIX.
****************************************************************
*    FILE = 倉庫マスタ                                       *
****************************************************************
 FD  ZSOKMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      ZSOKMS    OF   XFDLIB
                       JOINING   SOK       AS   PREFIX.
****************************************************************
*    FILE = 商品名称マスタ                                   *
****************************************************************
 FD  HMEIMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HMEIMS    OF   XFDLIB
                       JOINING   MEI       AS   PREFIX.
****************************************************************
*    FILE = 条件ファイル                                     *
****************************************************************
 FD  HJYOKEN
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HJYOKEN   OF   XFDLIB
                       JOINING   JYO       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  DSP-ST                   PIC  X(02).
     03  ZAN-ST                   PIC  X(02).
     03  HED-ST                   PIC  X(02).
     03  BDY-ST                   PIC  X(02).
     03  SHI-ST                   PIC  X(02).
     03  SOK-ST                   PIC  X(02).
     03  MEI-ST                   PIC  X(02).
     03  JYO-ST                   PIC  X(02).
*画面制御用領域
 01  DSP-CONTROL.
     03  DSP-FMT                  PIC  X(08).
     03  DSP-GRP                  PIC  X(08).
     03  DSP-PRO                  PIC  X(02).
     03  DSP-FNC                  PIC  X(04).
***  プログラムスイッチ（画面遷移制御）
 01  PSW                          PIC  X(01)  VALUE  SPACE.
*フラグ領域
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  INV-FLG                  PIC  9(01)  VALUE  ZERO.
     03  READ-FLG                 PIC  9(01)  VALUE  ZERO.
     03  ERR-FLG                  PIC  9(02)  VALUE  ZERO.
*カウント領域
 01  CNT-AREA.
     03  P-CNT                    PIC  9(07)  VALUE  ZERO.
     03  MAX-CNT                  PIC  9(07)  VALUE  ZERO.
     03  OUT-CNT                  PIC  9(07)  VALUE  ZERO.
     03  IX1                      PIC  9(02)  VALUE  ZERO.
*ワーク領域
 01  WRK-AREA.
     03  WK-YOTEIBI               PIC  9(08).
     03  WK-BDY-KEY.
         05  WK-BDY-F02           PIC  9(07).
     03  WK-SHOCD.
         05  WK-SHO               PIC  X(01)  OCCURS 8.
 01  WK-TANA.
     03  WK-TANA1                 PIC  X(01).
     03  FILLER                   PIC  X(01)  VALUE  "-".
     03  WK-TANA2                 PIC  X(03).
     03  FILLER                   PIC  X(01)  VALUE  "-".
     03  WK-TANA3                 PIC  X(02).
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
     03  PF-MSG1                  PIC  N(20)  VALUE
         NC"_取消_終了".
     03  PF-MSG2                  PIC  N(20)  VALUE
         NC"_取消_終了_前頁_次頁".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R   OCCURS   2    PIC   N(20).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1                 PIC  N(20)  VALUE
         NC"該当データは存在しません。".
     03  ERR-MSG2                 PIC  N(20)  VALUE
         NC"無効キーです。".
     03  ERR-MSG3                 PIC  N(20)  VALUE
         NC"前頁はありません。　　　".
     03  ERR-MSG4                 PIC  N(20)  VALUE
         NC"次頁はありません。　　　".
     03  ERR-MSG5                 PIC  N(20)  VALUE
         NC"倉庫コードが違います。".
     03  ERR-MSG6                 PIC  N(20)  VALUE
         NC"仕入先コードが違います。".
     03  ERR-MSG7                 PIC  N(20)  VALUE
         NC"商品コードが違います。".
     03  ERR-MSG8                 PIC  N(20)  VALUE
         NC"年月日に誤りがあります。".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  8    PIC  N(20).
*
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  DSP-ERR  PIC N(15) VALUE  NC"画面ファイルエラー".
     03  ZAN-ERR  PIC N(15) VALUE  NC"照会ワークＦエラー".
     03  HED-ERR  PIC N(15) VALUE  NC"発注Ｆ（ヘッダ）エラー".
     03  BDY-ERR  PIC N(15) VALUE  NC"発注Ｆ（明細）エラー".
     03  SHI-ERR  PIC N(15) VALUE  NC"仕入先マスタエラー".
     03  SOK-ERR  PIC N(15) VALUE  NC"倉庫マスタエラー".
     03  MEI-ERR  PIC N(15) VALUE  NC"商品名称マスタエラー".
     03  JYO-ERR  PIC N(15) VALUE  NC"条件ファイルエラー".
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
*
**************************************************************
 LINKAGE               SECTION.
**************************************************************
 01  LINK-SOKCD            PIC X(02).
 01  LINK-DSOKCD           PIC X(02).
**************************************************************
 PROCEDURE             DIVISION     USING LINK-SOKCD
                                          LINK-DSOKCD.
**************************************************************
 DECLARATIVES.
 DSP-SEC                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DSPFILE.
     MOVE        DSP-ST    TO        E-ST.
     MOVE        "DSPFILE" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     DSP-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 ZAN-SEC                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NZANWKF.
     MOVE        ZAN-ST    TO        E-ST.
     MOVE        "NZANWKF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     ZAN-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 HED-SEC                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HACHEDF.
     MOVE        HED-ST    TO        E-ST.
     MOVE        "HACHEDF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     HED-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 BDY-SEC                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HACMEIF.
     MOVE        BDY-ST    TO        E-ST.
     MOVE        "HACMEIF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     BDY-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SHI-SEC                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSHIMS.
     MOVE        SHI-ST    TO        E-ST.
     MOVE        "ZSHIMS"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SHI-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SOK-SEC                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE ZSOKMS.
     MOVE        SOK-ST    TO        E-ST.
     MOVE        "HMEIMS"  TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SOK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-SEC                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HMEIMS.
     MOVE        MEI-ST    TO        E-ST.
     MOVE        "MEIMS"   TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     MEI-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-SEC                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HJYOKEN.
     MOVE        JYO-ST    TO        E-ST.
     MOVE        "HJYOKEN" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JYO-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
******************TEST
     MOVE  "01"                   TO  LINK-SOKCD.
     MOVE  "01"                   TO  LINK-DSOKCD.
*
*    DISPLAY "倉庫CD    =" LINK-SOKCD.
*    DISPLAY "代表倉庫CD=" LINK-DSOKCD.
******************TEST
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
     OPEN      INPUT    NZANWKF  HJYOKEN.
     OPEN      INPUT    HACHEDF  HACMEIF  ZSHIMS  ZSOKMS  HMEIMS.
     OPEN      I-O      DSPFILE.
*ワークの初期化
     INITIALIZE                 FLG-AREA.
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
**
     EVALUATE      PSW
*初期画面表示
         WHEN      "1"       PERFORM   DSP-INIT-SEC
*ヘッダ部入力
         WHEN      "2"       PERFORM   DSP-HEAD-SEC
*確認入力
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
     CLOSE             DSPFILE  NZANWKF  HJYOKEN
                       HACHEDF  HACMEIF  ZSHIMS  ZSOKMS  HMEIMS.
**
 END-EXIT.
     EXIT.
****************************************************************
*             初期画面表示( PSW = 1 )                2.1       *
****************************************************************
 DSP-INIT-SEC          SECTION.
     MOVE     "DSP-INIT-SEC"      TO   S-NAME.
**
     MOVE     SPACE               TO   DSP-PRO.
*
     MOVE    SPACE                TO   DSP-FVD01601.
     MOVE    HEN-DATE             TO   DSP-SDATE.
     MOVE    HEN-TIME             TO   DSP-STIME.
     IF      LINK-DSOKCD   NOT =  "01"  AND  "88"
             MOVE    LINK-SOKCD        TO  SOK-F01  DSP-SOKCD
             PERFORM   ZSOKMS-READ-SEC
             IF      INV-FLG  =   ZERO
                     MOVE  SOK-F02     TO  DSP-SOKNM
             END-IF
     END-IF.
*項目属性クリア
     PERFORM  DSP-SYOKI-SEC.
*ヘッダ部入力へ
     MOVE     "2"                TO   PSW.
*
 DSP-INIT-EXIT.
     EXIT.
****************************************************************
*             ヘッダ部入力( PSW = 2 )                2.2       *
****************************************************************
 DSP-HEAD-SEC          SECTION.
     MOVE     "DSP-HEAD-SEC"      TO   S-NAME.
**
     PERFORM    DSP-WRITE-SEC.
     PERFORM    DSP-READ-SEC.
*
     EVALUATE   DSP-FNC
*実行
         WHEN   "E000"       PERFORM   HEAD-CHK-SEC
*取消
         WHEN   "F004"       MOVE    "1"      TO   PSW
*終了
         WHEN   "F005"       MOVE    "END"    TO   END-FLG
*他
         WHEN    OTHER       MOVE     2       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-HEAD-EXIT.
     EXIT.
****************************************************************
*             ヘッダ部チェック                 2.2.1           *
****************************************************************
 HEAD-CHK-SEC             SECTION.
     MOVE     "HEAD-CHK-SEC"     TO   S-NAME.
*倉庫コード
     IF       LINK-DSOKCD    =   "01"  OR  "88"
              MOVE    SPACE        TO  DSP-SOKNM
              MOVE    DSP-SOKCD    TO  SOK-F01
              PERFORM   ZSOKMS-READ-SEC
              IF   INV-FLG       =  ZERO
                   MOVE   SOK-F02  TO  DSP-SOKNM
              ELSE
                   MOVE  "R"       TO  EDIT-OPTION OF DSP-SOKCD
                   MOVE  "C"       TO  EDIT-CURSOR OF DSP-SOKCD
                   MOVE   5        TO  ERR-FLG
              END-IF
     END-IF.
*仕入先コード
     MOVE     SPACE              TO   DSP-HSIRNM.
     IF       DSP-HSIRCD   =     SPACE
              MOVE     SPACE     TO   DSP-HSIRCD
     END-IF.
     IF       DSP-HSIRCD   NOT =   SPACE
              MOVE    DSP-HSIRCD   TO  SHI-F01
              PERFORM   ZSHIMS-READ-SEC
              IF   INV-FLG       =  ZERO
                   MOVE   SHI-F02  TO  DSP-HSIRNM
              ELSE
                   MOVE  "R"       TO  EDIT-OPTION OF DSP-HSIRCD
                   MOVE  "C"       TO  EDIT-CURSOR OF DSP-HSIRCD
                IF     ERR-FLG   =  ZERO
                   MOVE   6        TO  ERR-FLG
                END-IF
              END-IF
     END-IF.
*商品コード
     MOVE     SPACE              TO   DSP-HSHONM.
     IF       DSP-HSHOCD   NOT =   SPACE
              MOVE    DSP-HSHOCD   TO  WK-SHOCD
              PERFORM              UNTIL WK-SHO(8) NOT = SPACE
                 PERFORM VARYING IX1 FROM 7 BY -1 UNTIL IX1 = 0
                         MOVE WK-SHO(IX1)  TO  WK-SHO(IX1 + 1)
                 END-PERFORM
                 MOVE    ZERO      TO  WK-SHO(1)
              END-PERFORM
              MOVE    WK-SHOCD     TO  DSP-HSHOCD
              MOVE    SPACE        TO  DSP-HSHONM
              MOVE    DSP-HSHOCD   TO  MEI-F011
              PERFORM   HMEIMS-START-SEC
              IF   INV-FLG       =  ZERO    AND
                   DSP-HSHOCD    =  MEI-F011
                   MOVE   MEI-F021 TO  DSP-HSHONM
              ELSE
                   MOVE  "R"       TO  EDIT-OPTION OF DSP-HSHOCD
                   MOVE  "C"       TO  EDIT-CURSOR OF DSP-HSHOCD
                IF     ERR-FLG   =  ZERO
                   MOVE   7        TO  ERR-FLG
                END-IF
              END-IF
     END-IF.
*入荷予定日
     IF       DSP-HYOTEI       =   ZERO
              MOVE  ZERO         TO    WK-YOTEIBI
     ELSE
              MOVE  "1"          TO    LINK-IN-KBN
              MOVE  DSP-HYOTEI   TO    LINK-IN-YMD6
              CALL  "SKYDTCKB"   USING LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD
              IF   LINK-OUT-RET  =  ZERO
                   MOVE  "3"          TO    LINK-IN-KBN
                   MOVE  DSP-HYOTEI   TO    LINK-IN-YMD6
                   CALL  "SKYDTCKB"   USING LINK-IN-KBN
                                            LINK-IN-YMD6
                                            LINK-IN-YMD8
                                            LINK-OUT-RET
                                            LINK-OUT-YMD
                   MOVE LINK-OUT-YMD  TO    WK-YOTEIBI
              ELSE
                   MOVE  "R"       TO  EDIT-OPTION OF DSP-HYOTEI
                   MOVE  "C"       TO  EDIT-CURSOR OF DSP-HYOTEI
                IF     ERR-FLG   =  ZERO
                   MOVE   8        TO  ERR-FLG
                END-IF
              END-IF
     END-IF.
*該当データ検索
     IF       ERR-FLG  =  ZERO
              PERFORM     HACHEDF-START-SEC
     END-IF.
*ワークへデータセット
     IF       ERR-FLG  =  ZERO
              PERFORM     MST-WORK-SEC
     END-IF.
*
     IF       ERR-FLG  =  1
              MOVE    "R"      TO   EDIT-OPTION OF DSP-SOKCD
              MOVE    "R"      TO   EDIT-OPTION OF DSP-HSIRCD
              MOVE    "R"      TO   EDIT-OPTION OF DSP-HSHOCD
              MOVE    "R"      TO   EDIT-OPTION OF DSP-HYOTEI
              MOVE    "C"      TO   EDIT-CURSOR OF DSP-SOKCD
     END-IF.
*
     IF       ERR-FLG  =  ZERO
              MOVE  "3"             TO   PSW
     END-IF.
 HEAD-CHK-EXIT.
     EXIT.
****************************************************************
*             該当データの検索                                 *
****************************************************************
 MST-WORK-SEC          SECTION.
     MOVE     "MST-WORK-SEC"   TO   S-NAME.
**
     CLOSE              NZANWKF.
     OPEN     OUTPUT    NZANWKF.
     MOVE     0                TO   OUT-CNT.
*対象データ抽出
     PERFORM             UNTIL  INV-FLG NOT = ZERO
              PERFORM    HACMEIF-START-SEC
              PERFORM    UNTIL  WK-BDY-KEY  = HIGH-VALUE
                    PERFORM   OUT-WRITE-SEC
                    PERFORM   HACMEIF-READ-SEC
              END-PERFORM
              PERFORM    HACHEDF-READ-SEC
     END-PERFORM.
*
     CLOSE              NZANWKF.
     OPEN     INPUT     NZANWKF.
     IF       OUT-CNT    =    0
              MOVE     1       TO   ERR-FLG
              GO       TO      MST-WORK-EXIT
     END-IF.
*初ページ編集
     COMPUTE   MAX-CNT  =  ( OUT-CNT + 6 )  /  7.
     MOVE      1               TO   P-CNT.
     MOVE      0               TO   READ-FLG.
     PERFORM   MEISAI-SET-SEC
               VARYING IX1 FROM 1 BY 1   UNTIL IX1 > 7.
 MST-WORK-EXIT.
     EXIT.
****************************************************************
*             ワークファイル出力                               *
****************************************************************
 OUT-WRITE-SEC         SECTION.
     MOVE     "OUT-WRITE-SEC"    TO   S-NAME.
*
     MOVE     SPACE              TO   ZAN-REC.
     INITIALIZE                       ZAN-REC.
*
     MOVE     HED-F17            TO   ZAN-F01.
     MOVE     HED-F02            TO   ZAN-F02.
     MOVE     BDY-F03            TO   ZAN-F03.
     MOVE     HED-F11            TO   ZAN-F04.
     MOVE     BDY-F06            TO   ZAN-F05.
     MOVE     BDY-F07            TO   ZAN-F06.
     MOVE     BDY-F08            TO   ZAN-F07.
     MOVE     HED-F06            TO   ZAN-F08.
     MOVE     BDY-F09            TO   ZAN-F09.
     MOVE     BDY-F10            TO   ZAN-F10.
     COMPUTE  ZAN-F11  =  BDY-F09  -  BDY-F10.
     MOVE     BDY-F21            TO   ZAN-F12.
     MOVE     BDY-F22            TO   ZAN-F13.
*
     MOVE     87                 TO   JYO-F01.
     MOVE     BDY-F22            TO   JYO-F02.
     READ     HJYOKEN
       INVALID KEY
               MOVE NC"＊＊＊＊＊＊"   TO   ZAN-F14
       NOT INVALID KEY
               MOVE JYO-F03            TO   ZAN-F14
     END-READ.
*
     WRITE    ZAN-REC.
     ADD      1                  TO   OUT-CNT.
 MST-WORK-EXIT.
     EXIT.
****************************************************************
*             ワーク→画面表示                                 *
****************************************************************
 MEISAI-SET-SEC        SECTION.
     MOVE     "MEISAI-SET-SEC"   TO   S-NAME.
*
     MOVE     SPACE              TO   DSP-MAS001(IX1).
     IF       READ-FLG      =  ZERO
              PERFORM  NZANWKF-READ-SEC
     END-IF.
     IF       READ-FLG  NOT =  ZERO
              GO     TO          MEISAI-SET-EXIT
     END-IF.
*
     MOVE     ZAN-F02            TO   DSP-HACNO (IX1).
     MOVE     ZAN-F03            TO   DSP-GYO   (IX1).
     MOVE     ZAN-F04            TO   DSP-YOTEI (IX1).
     MOVE     ZAN-F05            TO   DSP-SHOCD (IX1).
     MOVE     ZAN-F06(1:5)       TO   DSP-TAN1  (IX1).
     MOVE     ZAN-F06(6:2)       TO   DSP-TAN2  (IX1).
     MOVE     ZAN-F06(8:1)       TO   DSP-TAN3  (IX1).
     IF       ZAN-F07   NOT =  SPACE
              MOVE  ZAN-F07(1:1)      TO   WK-TANA1
              MOVE  ZAN-F07(2:3)      TO   WK-TANA2
              MOVE  ZAN-F07(5:2)      TO   WK-TANA3
              MOVE  WK-TANA           TO   DSP-TANA (IX1)
     END-IF.
     MOVE     ZAN-F09            TO   DSP-HACSU (IX1).
     MOVE     ZAN-F10            TO   DSP-NYKSU (IX1).
     MOVE     ZAN-F11            TO   DSP-HACZAN(IX1).
*
     MOVE     ZAN-F05            TO   MEI-F011.
     MOVE     ZAN-F06            TO   MEI-F012.
     PERFORM  HMEIMS-READ-SEC.
     IF       INV-FLG       =  ZERO
              MOVE   MEI-F021    TO   DSP-SHONM (IX1)
     END-IF.
*
     MOVE     ZAN-F08            TO   SHI-F01.
     PERFORM  ZSHIMS-READ-SEC.
     IF       INV-FLG       =  ZERO
              MOVE   SHI-F02     TO   DSP-SIRNM (IX1)
     END-IF.
     MOVE     ZAN-F12            TO   DSP-DBNG  (IX1).
     MOVE     ZAN-F14            TO   DSP-DTSYU (IX1).
*
 MEISAI-SET-EXIT.
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
         WHEN   "E000"       MOVE    "3"      TO   PSW
*取消
         WHEN   "F004"       MOVE    "1"      TO   PSW
*終了
         WHEN   "F005"       MOVE    "END"    TO   END-FLG
*前頁
         WHEN   "F011"       PERFORM  ZEN-PAGE-SEC
*次頁
         WHEN   "F012"       PERFORM  JI-PAGE-SEC
*他
         WHEN   OTHER        MOVE     2       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
****************************************************************
*             前ページ                                         *
****************************************************************
 ZEN-PAGE-SEC          SECTION.
     MOVE     "ZEN-PAGE-SEC"      TO   S-NAME.
**
     IF        P-CNT     =    1
               MOVE    3          TO   ERR-FLG
               GO      TO         ZEN-PAGE-EXIT
     END-IF.
*
     MOVE      DSP-SOKCD          TO   ZAN-F01.
     MOVE      DSP-HACNO (1)      TO   ZAN-F02.
     MOVE      DSP-GYO   (1)      TO   ZAN-F03.
     START     NZANWKF   KEY IS   <    ZAN-F01
                                       ZAN-F02
                                       ZAN-F03
               WITH REVERSED ORDER
        INVALID
               MOVE     3         TO    ERR-FLG
               GO       TO        ZEN-PAGE-EXIT
     END-START.
*
     SUBTRACT  1             FROM   P-CNT.
     MOVE      0               TO   READ-FLG.
     PERFORM   MEISAI-SET-SEC
               VARYING IX1 FROM 7 BY -1  UNTIL IX1 = 0.
*
 ZEN-PAGE-EXIT.
     EXIT.
****************************************************************
*             次ページ                                         *
****************************************************************
 JI-PAGE-SEC           SECTION.
     MOVE     "JI-PAGE-SEC"       TO   S-NAME.
**
     IF        P-CNT     =    MAX-CNT
               MOVE    4          TO   ERR-FLG
               GO      TO         JI-PAGE-EXIT
     END-IF.
*
     MOVE      DSP-SOKCD          TO   ZAN-F01.
     MOVE      DSP-HACNO (7)      TO   ZAN-F02.
     MOVE      DSP-GYO   (7)      TO   ZAN-F03.
     START     NZANWKF   KEY IS   >    ZAN-F01
                                       ZAN-F02
                                       ZAN-F03
        INVALID
               MOVE     4         TO    ERR-FLG
               GO       TO        JI-PAGE-EXIT
     END-START.
*
     ADD       1               TO   P-CNT.
     MOVE      0               TO   READ-FLG.
     PERFORM   MEISAI-SET-SEC
               VARYING IX1 FROM 1 BY 1   UNTIL IX1 > 7.
*
 JI-PAGE-EXIT.
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
         WHEN   "2"
                MOVE    PF-MSG-R(1)        TO   DSP-PFGAID
         WHEN   "3"
                MOVE    PF-MSG-R(2)        TO   DSP-PFGAID
     END-EVALUATE.
*画面の表示
     MOVE    "SCREEN"            TO   DSP-GRP.
     MOVE    "FVD01601"          TO   DSP-FMT.
     WRITE    DSP-FVD01601.
*
 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*             画面読込処理                                     *
****************************************************************
 DSP-READ-SEC          SECTION.
     MOVE     "DSP-READ-SEC"      TO   S-NAME.
**
     MOVE    "NE"                 TO   DSP-PRO.
     EVALUATE   PSW
         WHEN   "2"
                MOVE    "HEAD"    TO   DSP-GRP
         WHEN   "3"
                MOVE    "KAKU"    TO   DSP-GRP
     END-EVALUATE.
     MOVE    "FVD01601"           TO   DSP-FMT.
     READ    DSPFILE.
*
 DSP-READ-010.
     PERFORM  DSP-SYOKI-SEC.
     MOVE    ZERO                 TO   ERR-FLG.
     MOVE    SPACE                TO   DSP-PRO.
*
 DSP-READ-EXIT.
     EXIT.
****************************************************************
*             画面制御項目初期化                               *
****************************************************************
 DSP-SYOKI-SEC         SECTION.
     MOVE     "DSP-SYOKI-SEC"    TO   S-NAME.
**
     MOVE   "M"        TO  EDIT-OPTION OF DSP-SOKCD.
     MOVE   "M"        TO  EDIT-OPTION OF DSP-HSIRCD.
     MOVE   "M"        TO  EDIT-OPTION OF DSP-HSHOCD.
     MOVE   "M"        TO  EDIT-OPTION OF DSP-HYOTEI.
     MOVE   SPACE      TO  EDIT-CURSOR OF DSP-SOKCD.
     MOVE   SPACE      TO  EDIT-CURSOR OF DSP-HSIRCD.
     MOVE   SPACE      TO  EDIT-CURSOR OF DSP-HSHOCD.
     MOVE   SPACE      TO  EDIT-CURSOR OF DSP-HYOTEI.
*
     IF     LINK-DSOKCD    NOT =  "01"  AND  "88"
            MOVE   "X"     TO  EDIT-STATUS OF DSP-SOKCD
     END-IF.
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             発注残ワーク・読み込み                           *
****************************************************************
 NZANWKF-READ-SEC      SECTION.
     MOVE     "NZANWKF-READ-SEC"  TO   S-NAME.
**
     READ     NZANWKF
         AT END     MOVE  1       TO   READ-FLG
     END-READ.
*
 NZANWKF-READ-EXIT.
     EXIT.
****************************************************************
*             発注ファイル（ヘッダ）スタート                   *
****************************************************************
 HACHEDF-START-SEC     SECTION.
     MOVE     "HACHEDF-START-SEC" TO   S-NAME.
**
     MOVE     0                   TO   INV-FLG.
     MOVE     DSP-SOKCD           TO   HED-F17.
     MOVE     DSP-HSIRCD          TO   HED-F06.
     MOVE     0                   TO   HED-F02.
     START    HACHEDF    KEY IS   >=   HED-F17
*]]]                                      HED-F06
                                       HED-F02
          INVALID    MOVE    1    TO   INV-FLG
          NOT INVALID
                     PERFORM   HACHEDF-READ-SEC
     END-START.
*
     IF       INV-FLG   NOT =  ZERO
                     MOVE    1    TO   ERR-FLG
     END-IF.
 HACHEDF-START-EXIT.
     EXIT.
****************************************************************
*             発注ファイル（ヘッダ）読み込み                   *
****************************************************************
 HACHEDF-READ-SEC      SECTION.
     MOVE     "HACHEDF-READ-SEC"  TO   S-NAME.
**
     MOVE     0                   TO   INV-FLG.
     READ     HACHEDF
         AT END     MOVE  1       TO   INV-FLG
                    GO    TO      HACHEDF-READ-EXIT
     END-READ.
*倉庫ＣＤ
 HACHEDF-READ-010.
     IF       DSP-SOKCD   NOT =  HED-F17
                    MOVE  1       TO   INV-FLG
                    GO    TO      HACHEDF-READ-EXIT
     END-IF.
*仕入先ＣＤ
 HACHEDF-READ-020.
     IF       DSP-HSIRCD  NOT =  SPACE   AND
              DSP-HSIRCD  NOT =  HED-F06
******              MOVE  1       TO   INV-FLG
******              GO    TO      HACHEDF-READ-EXIT
                    GO    TO      HACHEDF-READ-SEC
     END-IF.
*仕入予定日
 HACHEDF-READ-030.
     IF       WK-YOTEIBI  NOT =  ZERO    AND
              WK-YOTEIBI  NOT =  HED-F11
                    GO    TO      HACHEDF-READ-SEC
     END-IF.
 HACHEDF-READ-040.
*伝票区分
     IF       HED-F01     NOT =  50
                    GO    TO      HACHEDF-READ-SEC
     END-IF.
 HACHEDF-READ-050.
*枝番・完了区分・取消フラグ
     IF       HED-F03     NOT =  ZERO    OR
              HED-F04     NOT =  ZERO    OR
              HED-F24     NOT =  ZERO
                    GO    TO      HACHEDF-READ-SEC
     END-IF.
 HACHEDF-READ-060.
*発注ＦＬＧ
     IF       HED-F25     NOT =  1
                    GO    TO      HACHEDF-READ-SEC
     END-IF.
 HACHEDF-READ-EXIT.
     EXIT.
****************************************************************
*             発注ファイル（明細）スタート                     *
****************************************************************
 HACMEIF-START-SEC     SECTION.
     MOVE     "HACMEIF-START-SEC" TO   S-NAME.
**
     MOVE     HED-F02             TO   BDY-F02  WK-BDY-F02.
     MOVE     0                   TO   BDY-F03.
     START    HACMEIF    KEY IS   >=   BDY-F02
                                       BDY-F03
          INVALID    MOVE  HIGH-VALUE   TO  WK-BDY-KEY
          NOT INVALID
                     PERFORM    HACMEIF-READ-SEC
     END-START.
 HACMEIF-START-EXIT.
     EXIT.
****************************************************************
*             発注ファイル（明細）読み込み                     *
****************************************************************
 HACMEIF-READ-SEC      SECTION.
     MOVE     "HACMEIF-READ-SEC"  TO   S-NAME.
**
     READ     HACMEIF
         AT END     MOVE  HIGH-VALUE   TO  WK-BDY-KEY
                    GO    TO      HACMEIF-READ-EXIT
     END-READ.
 HACMEIF-READ-010.
*伝票番号
     IF       HED-F02     NOT =  BDY-F02
                    MOVE  HIGH-VALUE   TO  WK-BDY-KEY
                    GO    TO      HACMEIF-READ-EXIT
     END-IF.
 HACMEIF-READ-020.
*商品CD
     IF       DSP-HSHOCD  NOT =  SPACE   AND
              DSP-HSHOCD  NOT =  BDY-F06
                    GO    TO      HACMEIF-READ-SEC
     END-IF.
 HACMEIF-READ-030.
*完了区分
     IF       BDY-F05         =  1
                    GO    TO      HACMEIF-READ-SEC
     END-IF.
 HACMEIF-READ-040.
*発注数＝入荷数
     IF       BDY-F09         =  BDY-F10
                    GO    TO      HACMEIF-READ-SEC
     END-IF.
 HACMEIF-READ-EXIT.
     EXIT.
****************************************************************
*             仕入先マスタ読み込み                             *
****************************************************************
 ZSHIMS-READ-SEC       SECTION.
     MOVE     "ZSHIMS-READ-SEC"   TO   S-NAME.
**
     MOVE     0                   TO   INV-FLG.
     READ     ZSHIMS
         INVALID    MOVE  1       TO   INV-FLG
     END-READ.
 ZSHIMS-READ-EXIT.
     EXIT.
****************************************************************
*             倉庫マスタ読み込み                               *
****************************************************************
 ZSOKMS-READ-SEC       SECTION.
     MOVE     "ZSOKMS-READ-SEC"   TO   S-NAME.
**
     MOVE     0                   TO   INV-FLG.
     READ     ZSOKMS
         INVALID    MOVE  1       TO   INV-FLG
     END-READ.
 ZSOKMS-READ-EXIT.
     EXIT.
****************************************************************
*             商品マスタ・スタート                             *
****************************************************************
 HMEIMS-START-SEC      SECTION.
     MOVE     "HMEIMS-START-SEC"  TO   S-NAME.
**
     MOVE     0                   TO   INV-FLG.
     MOVE     SPACE               TO   MEI-F012.
     START    HMEIMS     KEY IS   >=   MEI-F011
                                       MEI-F012
          INVALID    MOVE    1    TO   INV-FLG
                     GO      TO        HMEIMS-START-EXIT
     END-START.
*
     READ     HMEIMS   NEXT
          AT END     MOVE    1    TO   INV-FLG
     END-READ.
 HMEIMS-START-EXIT.
     EXIT.
****************************************************************
*             商品マスタ・読み込み                             *
****************************************************************
 HMEIMS-READ-SEC       SECTION.
     MOVE     "HMEIMS-READ-SEC"   TO   S-NAME.
**
     MOVE     0                   TO   INV-FLG.
     READ     HMEIMS
         INVALID    MOVE  1       TO   INV-FLG
     END-READ.
 HMEIMS-READ-EXIT.
     EXIT.
*******************< PROGRAM-END NVD0160I >*********************

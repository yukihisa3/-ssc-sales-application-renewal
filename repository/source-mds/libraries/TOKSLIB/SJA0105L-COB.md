# SJA0105L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SJA0105L.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＪＡＮ管理システム　　　　　　　　*
*    モジュール名　　　　：　ＪＡＮ条件マスタリスト　　　　　　*
*    作成日／更新日　　　：　99/10/28                          *
*    作成者／更新者　　　：　ＮＡＶ萩原                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SJA0105L.
 AUTHOR.               HAGIWARA.
 DATE-WRITTEN.         99/10/28.
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
*ＪＡＮ条件マスタ
     SELECT  JANJYOF   ASSIGN    TO        JANJYOL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       JYO-F01
                                           JYO-F02
                       FILE      STATUS    JYO-ST.
*プリント定義ファイル
     SELECT  PRTFILE   ASSIGN    TO        LP-04-PRTF
                       FILE      STATUS    PRT-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = ＪＡＮ条件マスタ                                   *
****************************************************************
 FD  JANJYOF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JANJYOF   OF   XFDLIB
                       JOINING   JYO       AS   PREFIX.
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
     03  JYO-ST                   PIC  X(02).
     03  PRT-ST                   PIC  X(02).
*フラグ領域
 01  FLG-AREA.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
*ワーク領域
 01  WRK-AREA.
***  ラインカウント　
     03  L-CNT                    PIC  9(02)  VALUE  ZERO.
***  ページカウント　
     03  P-CNT                    PIC  9(03)  VALUE  ZERO.
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
*
 01  FILE-ERR.
     03  JYO-ERR           PIC N(20) VALUE
                        NC"ＪＡＮ条件マスタエラー".
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
         NC"※※　ＪＡＮ条件マスタリスト　※※"
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
     03  FILLER                  PIC  X(16)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                             NC"条件区分".
     03  FILLER                  PIC  X(09)  VALUE  SPACE.
     03  FILLER                  PIC  N(02)  VALUE  NC"条件".
     03  FILLER                  PIC  X(04)  VALUE  "ｺｰﾄﾞ".
     03  FILLER                  PIC  X(08)  VALUE  SPACE.
     03  FILLER                  PIC  N(02)  VALUE  NC"名称".
     03  FILLER                  PIC  X(43)  VALUE  SPACE.
     03  FILLER                  PIC  X(02)  VALUE  "ｶﾅ".
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
 01  DT1                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(16)  VALUE  SPACE.
     03  DT1-01                  PIC  X(03).
     03  FILLER                  PIC  X(14)  VALUE  SPACE.
     03  DT1-02                  PIC  9(10).
     03  FILLER                  PIC  X(06)  VALUE  SPACE.
     03  DT1-03                  PIC  N(20).
     03  FILLER                  PIC  X(07)  VALUE  SPACE.
     03  DT1-04                  PIC  X(20).
*
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 JYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JANJYOF.
     DISPLAY     JYO-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     JYO-ST    UPON      CONS.
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
     OPEN     INPUT     JANJYOF.
     OPEN     OUTPUT    PRTFILE.
*ワークの初期化
     INITIALIZE         FLG-AREA.
*
     PERFORM  HEAD-WRITE-SEC.
*
     PERFORM  FL-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             2.0
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*改ページ判定
     IF       L-CNT     >=   62
              MOVE      SPACE    TO    PRT-REC
              WRITE     PRT-REC  AFTER PAGE
              MOVE      ZERO     TO    L-CNT
              PERFORM   HEAD-WRITE-SEC
     END-IF.
*
     PERFORM   BODY-WRITE-SEC
*
     PERFORM  FL-READ-SEC.
*
 MAIN-EXIT.
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
     MOVE     "SJA0105L"          TO        HD1-01.
***  日付
     MOVE     HEN-DATE-YYYY       TO        HD1-02.
     MOVE     HEN-DATE-MM         TO        HD1-03.
     MOVE     HEN-DATE-DD         TO        HD1-04.
***  ページ_
     MOVE     P-CNT               TO        HD1-05.
*
*ヘッダ部出力
     WRITE    PRT-REC      FROM   HD1       AFTER  2.
     WRITE    PRT-REC      FROM   SEN       AFTER  2.
     WRITE    PRT-REC      FROM   HD2       AFTER  1.
     WRITE    PRT-REC      FROM   SEN       AFTER  1.
*
     ADD      6            TO        L-CNT.
 HEAD-WRITE-EXIT.
     EXIT.
****************************************************************
*             明細部出力処理　                       2.4.2     *
****************************************************************
 BODY-WRITE-SEC              SECTION.
     MOVE     "BODY-WRITE-SEC"    TO   S-NAME.
*
     MOVE     SPACE          TO   DT1.
***  条件区分
     MOVE     JYO-F01        TO   DT1-01.
***  条件コード
     MOVE     JYO-F02        TO   DT1-02.
***  名称
     MOVE     JYO-F03        TO   DT1-03.
***  カナ
     MOVE     JYO-F04        TO   DT1-04.
*
*明細部出力
*
     WRITE    PRT-REC   FROM DT1  AFTER  1.
     WRITE    PRT-REC   FROM SEN1 AFTER  1.
*
     ADD      2            TO        L-CNT.
*
 BODY-WRITE-EXIT.
     EXIT.
****************************************************************
*             ＪＡＮ条件マスタＲＥＡＤ                         *
****************************************************************
 FL-READ-SEC           SECTION.
     MOVE     "FL-READ-SEC"       TO   S-NAME.
*
     READ     JANJYOF        AT   END
              MOVE     "END"      TO   END-FLG
              GO        TO        FL-READ-EXIT
     END-READ.
*
 FL-READ-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
*ファイル ＣＬＯＳＥ
     CLOSE             JANJYOF  PRTFILE.
**
 END-EXIT.
     EXIT.
*****************<<  SJA0105L   END PROGRAM  >>******************

```

# SJA01021

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SJA01021.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    モジュール名　　　　：　ＪＡＮマスタ照会                  *
*    作成日／作成者　　　：　1999/10/28  /HAGIWARA             *
*    更新日／更新者　　　：　                                  *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SJA0102I.
 AUTHOR.               HAGIWARA.
 DATE-WRITTEN.         99/10/28.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*ＪＡＮ管理ファイル１（KEY:F01 JANｺｰﾄﾞ)
     SELECT  JANKANF1  ASSIGN    TO        JANKANL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       KA1-F01
                       FILE      STATUS    KA1-ST.
*ＪＡＮ管理ファイル２（KEY:F04 ﾋﾝﾒｲｶﾅ)
     SELECT  JANKANF2  ASSIGN    TO        JANKANL2
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       KA2-F04
                       FILE      STATUS    KA2-ST.
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
*    FILE = ＪＡＮ管理ファイル１                               *
****************************************************************
 FD  JANKANF1
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JANKANL1  OF   XFDLIB
                       JOINING   KA1       AS   PREFIX.
****************************************************************
*    FILE = ＪＡＮ管理ファイル２                               *
****************************************************************
 FD  JANKANF2
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JANKANL2  OF   XFDLIB
                       JOINING   KA2       AS   PREFIX.
****************************************************************
*    FILE = 画面ファイル                                       *
****************************************************************
 FD  DSPFILE
                       LABEL     RECORD    IS   OMITTED.
                       COPY      FJA01021  OF   XMDLIB
                       JOINING   DSP       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  KA1-ST                   PIC  X(02).
     03  KA2-ST                   PIC  X(02).
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
     03  RD-FILE-FLG              PIC  9(01)  VALUE  ZERO.
     03  INV-FLG                  PIC  9(01)  VALUE  ZERO.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  RD-FLG                   PIC  X(03)  VALUE  SPACE.
*ワーク領域
 01  WRK-AREA.
***  プログラムスイッチ（画面遷移制御）
     03  PSW                      PIC  X(01)  VALUE  SPACE.
*
     03  IX                       PIC  9(02)  VALUE  ZERO.
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
 01  WK-JIKAN.
     03  WK-HH                    PIC  9(02)  VALUE  ZERO.
     03  WK-MM                    PIC  9(02)  VALUE  ZERO.
*
*ＰＦガイド
 01  PF-MSG-AREA.
     03  PF-MSG1.
         05  FILLER               PIC   N(20)
      VALUE NC"_取消_終了　　　　　　　　　　　".
     03  PF-MSG2.
         05  FILLER               PIC   N(20)
      VALUE NC"_取消_終了".
**** 03  PF-MSG2.
****     05  FILLER               PIC   N(20)
****  VALUE NC"_取消_終了_項目戻し".
 01  PF-MSG-AREA-R       REDEFINES     PF-MSG-AREA.
     03  PF-MSG-R   OCCURS   2   PIC   N(20).
*
*メッセージの取得
 01  ERR-MSG-AREA.
     03  ERR-MSG1.
         05  FILLER              PIC   N(20)
             VALUE NC"ＪＡＮコード又は品名を入力して下さい".
     03  ERR-MSG2.
         05  FILLER              PIC   N(20)
             VALUE NC"キーが無効です".
     03  ERR-MSG3.
         05  FILLER              PIC   N(20)
             VALUE NC"該当するデータが存在しません".
 01  ERR-MSG-AREA-R      REDEFINES     ERR-MSG-AREA.
     03  ERR-MSG-R   OCCURS  3   PIC   N(20).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  KA1-ERR           PIC N(15) VALUE
                        NC"ＪＡＮ管理Ｆ１エラー".
     03  KA2-ERR           PIC N(15) VALUE
                        NC"ＪＡＮ管理Ｆ２エラー".
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
*
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 KA1-ERR                    SECTION.
     USE         AFTER      EXCEPTION PROCEDURE JANKANF1.
     MOVE        KA1-ST     TO        E-ST.
     MOVE        "JANKANF1" TO        E-FILE.
     DISPLAY     SEC-NAME   UPON      CONS.
     DISPLAY     ERR-FILE   UPON      CONS.
     DISPLAY     ERR-NAME   UPON      CONS.
     DISPLAY     KA1-ERR    UPON      CONS.
     MOVE        "4000"     TO        PROGRAM-STATUS.
     STOP        RUN.
 KA2-ERR                    SECTION.
     USE         AFTER      EXCEPTION PROCEDURE JANKANF2.
     MOVE        KA2-ST     TO        E-ST.
     MOVE        "JANKANF2" TO        E-FILE.
     DISPLAY     SEC-NAME   UPON      CONS.
     DISPLAY     ERR-FILE   UPON      CONS.
     DISPLAY     ERR-NAME   UPON      CONS.
     DISPLAY     KA2-ERR    UPON      CONS.
     MOVE        "4000"     TO        PROGRAM-STATUS.
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
*システム時刻取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*ファイルのＯＰＥＮ
     OPEN      INPUT            JANKANF1  JANKANF2.
     OPEN      I-O              DSPFILE.
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
*
     EVALUATE      PSW
*処理区分入力(2.1)
         WHEN      "1"       PERFORM   DSP-INIT-SEC
*ヘッダ部入力(2.2)
         WHEN      "2"       PERFORM   DSP-HEAD-SEC
*確認入力    (2.3)
         WHEN      "3"       PERFORM   DSP-KAKU-SEC
*
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
     CLOSE             DSPFILE   JANKANF1   JANKANF2.
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
     MOVE    SPACE                TO   DSP-FJA01021.
*システム日付転送
     MOVE    HEN-DATE             TO   DSP-SDATE.
*システム時間転送
     MOVE    HEN-TIME             TO   DSP-STIME.
*項目属性クリア
     PERFORM  DSP-SYOKI-SEC.
*ヘッダ部入力へ
     MOVE    "2"                  TO   PSW.
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
************************* MOVE      SPACE       TO   DSP-FJA01021
***                ＪＡＮコード順
                   IF     RD-FILE-FLG =    1
                          PERFORM     BODY-WRITE1-SEC
***                品名カナ順
                   ELSE
                          PERFORM     BODY-WRITE2-SEC
                   END-IF
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
*             ヘッダチェック                         2.2.1     *
****************************************************************
 HEAD-CHK-SEC             SECTION.
     MOVE     "HEAD-CHK-SEC"     TO   S-NAME.
*
***  ＪＡＮコード，品名カナ共未入力時エラー
     IF     ( DSP-HJANCD     NOT  NUMERIC
         OR   DSP-HJANCD     =    ZERO    )
        AND ( DSP-HKANA      =    SPACE   )
              MOVE           1    TO   ERR-FLG
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-HJANCD
              MOVE     "R"   TO   EDIT-OPTION  OF  DSP-HKANA
              MOVE     "C"   TO   EDIT-CURSOR  OF  DSP-HJANCD
              GO   TO   HEAD-CHK-EXIT
     ELSE
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-HJANCD
              MOVE     "M"   TO   EDIT-OPTION  OF  DSP-HKANA
              MOVE     SPACE TO   EDIT-CURSOR  OF  DSP-HJANCD
     END-IF.
*ＪＡＮ管理ファイル存在チェック
***  ＪＡＮコード入力済み時，ＪＡＮコード順
     IF       DSP-HJANCD     NUMERIC
         AND  DSP-HJANCD     NOT =  ZERO
              PERFORM   JANKANF1-START-SEC
              IF   INV-FLG   = ZERO
                   MOVE     "M"   TO   EDIT-OPTION  OF  DSP-HJANCD
                   MOVE     SPACE TO   EDIT-CURSOR  OF  DSP-HJANCD
                   MOVE      1    TO   RD-FILE-FLG
                   PERFORM   JANKANF1-READ-SEC
              ELSE
                   IF   ERR-FLG   =    ZERO
                        MOVE      3    TO   ERR-FLG
                        MOVE  "R"  TO  EDIT-OPTION  OF  DSP-HJANCD
                        MOVE  "C"  TO  EDIT-CURSOR  OF  DSP-HJANCD
                   END-IF
                   GO             TO   HEAD-CHK-EXIT
              END-IF

***  ＪＡＮコード未入力，品名カナ入力時，品名カナ順
     ELSE
         IF   DSP-HKANA      NOT  =  SPACE
              PERFORM   JANKANF2-START-SEC
              IF   INV-FLG   = ZERO
                   MOVE     "M"   TO   EDIT-OPTION  OF  DSP-HKANA
                   MOVE     SPACE TO   EDIT-CURSOR  OF  DSP-HKANA
                   MOVE      2    TO   RD-FILE-FLG
                   PERFORM   JANKANF2-READ-SEC
              ELSE
                   IF   ERR-FLG   =    ZERO
                        MOVE      3    TO   ERR-FLG
                        MOVE  "R"  TO  EDIT-OPTION  OF  DSP-HKANA
                        MOVE  "C"  TO  EDIT-CURSOR  OF  DSP-HKANA
                   END-IF
                   GO             TO   HEAD-CHK-EXIT
              END-IF
         END-IF
     END-IF.
 HEAD-CHK-EXIT.
     EXIT.
****************************************************************
*             明細出力処理　JANKANF1                          *
****************************************************************
 BODY-WRITE1-SEC         SECTION.
     MOVE     "BODY-WRITE1-SEC"   TO   S-NAME.
*
*    ＪＡＮ管理マスタを読みながら明細項目をセット
     PERFORM  VARYING IX FROM 1 BY 1 UNTIL IX > 8
                                     OR  RD-FLG = "END"
***     ＪＡＮコード
        MOVE     KA1-F01          TO   DSP-MJANCD(IX)
***     品名
        MOVE     KA1-F03(1:10)    TO   DSP-HINNM1(IX)
        MOVE     KA1-F03(11:10)   TO   DSP-HINNM2(IX)
***     カナ
        MOVE     KA1-F04          TO   DSP-MKANA(IX)
***
        PERFORM  JANKANF1-READ-SEC
     END-PERFORM.
*
 BODY-WRITE1-EXIT.
     EXIT.
****************************************************************
*             明細出力処理　JANKANF2                          *
****************************************************************
 BODY-WRITE2-SEC         SECTION.
     MOVE     "BODY-WRITE2-SEC"   TO   S-NAME.
*
*    ＪＡＮ管理マスタを読みながら明細項目をセット
     PERFORM  VARYING IX FROM 1 BY 1 UNTIL IX > 8
                                     OR  RD-FLG = "END"
***     ＪＡＮコード
        MOVE     KA2-F01          TO   DSP-MJANCD(IX)
***     品名
        MOVE     KA2-F03(1:10)    TO   DSP-HINNM1(IX)
        MOVE     KA2-F03(11:10)   TO   DSP-HINNM2(IX)
***     カナ
        MOVE     KA2-F04          TO   DSP-MKANA(IX)
***
        PERFORM  JANKANF2-READ-SEC
     END-PERFORM.
*
 BODY-WRITE2-EXIT.
     EXIT.
****************************************************************
*             確認処理　入力（ PSW = 3 ）            2.3
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
***             項目初期化
                MOVE    SPACE    TO   RD-FLG
                MOVE    ZERO     TO   IX
***
                CLOSE   JANKANF1 JANKANF2
                OPEN    INPUT    JANKANF1 JANKANF2
*終了
         WHEN   "F005"
                MOVE    "END"    TO   END-FLG
*項目戻し
******** WHEN   "F006"
********        MOVE    "2"      TO   PSW
***             項目初期化
********        MOVE    SPACE    TO   RD-FLG
********        MOVE    ZERO     TO   IX
***
********        CLOSE   JANKANF1 JANKANF2
********        OPEN    INPUT    JANKANF1 JANKANF2
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
*          MOVE    ZERO               TO   ERR-FLG
     END-IF.
*ガイドメッセージの設定
     EVALUATE      PSW
         WHEN      "1"  WHEN  "2"
                   MOVE    PF-MSG-R(1)     TO   DSP-PFGAID
         WHEN      "3"
                   MOVE    PF-MSG-R(2)     TO   DSP-PFGAID
     END-EVALUATE.
*画面の表示
*****IF      PSW  =  "3"
*****MOVE    "BODY"            TO   DSP-GRP
*****ELSE
     MOVE    "SCREEN"            TO   DSP-GRP.
*****END-IF.
     MOVE    "FJA01021"          TO   DSP-FMT.
     WRITE    DSP-FJA01021.
     IF    ERR-FLG = ZERO
           PERFORM  DSP-SYOKI-SEC
     END-IF.
     MOVE     ZERO               TO   ERR-FLG.
*
 DSP-WRITE-EXIT.
     EXIT.
****************************************************************
*             画面読込処理                                     *
****************************************************************
 DSP-READ-SEC          SECTION.
     MOVE     "DSP-READ-SEC"      TO   S-NAME.
*
*****IF      PSW  =  "2"
*****        MOVE    SPACE                TO   DSP-PRO
*****ELSE
     MOVE    "NE"                 TO   DSP-PRO
*****END-IF.
*
     EVALUATE   PSW
*ヘッダ部
         WHEN   "2"
                MOVE    "HEAD"    TO   DSP-GRP
*確認
         WHEN   "3"
                MOVE    "KAKU"    TO   DSP-GRP
     END-EVALUATE.

     MOVE    "FJA01021"           TO   DSP-FMT.
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
***  バッチ_
     MOVE "M"   TO EDIT-OPTION OF DSP-HJANCD.
     MOVE "M"   TO EDIT-OPTION OF DSP-HKANA.
     MOVE SPACE TO EDIT-CURSOR OF DSP-HJANCD.
     MOVE SPACE TO EDIT-CURSOR OF DSP-HKANA.
*
 DSP-SYOKI-EXIT.
     EXIT.
****************************************************************
*             ＪＡＮ管理ファイル１　ＳＴＡＲＴ                 *
****************************************************************
 JANKANF1-START-SEC     SECTION.
     MOVE     "JANKANF1-START-SEC"  TO   S-NAME.
*
     MOVE     DSP-HJANCD          TO   KA1-F01.
     START    JANKANF1  KEY  IS   >=   KA1-F01
       INVALID
              MOVE      9    TO   INV-FLG
       NOT INVALID
              MOVE      ZERO TO   INV-FLG
     END-START.
*
 JANKANF1-START-EXIT.
     EXIT.
****************************************************************
*             ＪＡＮ管理ファイル２　ＳＴＡＲＴ                 *
****************************************************************
 JANKANF2-START-SEC     SECTION.
     MOVE     "JANKANF2-START-SEC"  TO   S-NAME.
*
     MOVE     DSP-HKANA           TO   KA2-F04.
     START    JANKANF2  KEY  IS   >=   KA2-F04
       INVALID
              MOVE      9    TO   INV-FLG
       NOT INVALID
              MOVE      ZERO TO   INV-FLG
     END-START.
*
 JANKANF2-START-EXIT.
     EXIT.
****************************************************************
*             ＪＡＮ管理ファイル１　ＲＥＡＤ                   *
****************************************************************
 JANKANF1-READ-SEC      SECTION.
     MOVE     "JANKANF1-READ-SEC"  TO   S-NAME.
*マスタ読込み
     READ JANKANF1 AT END
          MOVE   "END"    TO   RD-FLG

     END-READ.
*
 JANKANF1-READ-EXIT.
     EXIT.
****************************************************************
*             ＪＡＮ管理ファイル２　ＲＥＡＤ                   *
****************************************************************
 JANKANF2-READ-SEC      SECTION.
     MOVE     "JANKANF2-READ-SEC"  TO   S-NAME.
*マスタ読込み
     READ JANKANF2 AT END
          MOVE   "END"    TO   RD-FLG
     END-READ.
*
 JANKANF2-READ-EXIT.
     EXIT.
*******************< PROGRAM-END SJA0102I >*********************

```

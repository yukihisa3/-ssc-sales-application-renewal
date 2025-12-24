# SNJ8000B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SNJ8000B.COB`

## ソースコード

```cobol
****************************************************************
*    2010/09/03 新受配信システムへ切り替え                     *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　受配信管理システム　　　　　　　　*
*    モジュール名　　　　：　ＶＬＤ受取り／売上更新実行        *
*    作成日／更新日　　　：　2010/09/03                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＶＬＤを読込み、売上更新処理を実行*
*                            する。（バック処理にて実行）      *
*                            ジョブ名称は回線毎に変更する。    *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SNJ8000B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/09/03.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS.
*--------------------------------------------------------------*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  ＶＬＤ５００  >>---*
     SELECT   VLD500    ASSIGN    TO        VLD500
                        FILE      STATUS    IS    VLD1-ST.
*---<<  実行制御マスタ  >>---*
     SELECT  JSMJIKF    ASSIGN    TO        DA-01-VI-JSMJIKL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JIK-F01
                        FILE      STATUS    IS   JIK-ST.
*--------------------------------------------------------------*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  ＶＬＤ５００  >>---*
 FD  VLD500.
 01  VLD1-REC.
     03  VLD1-F01          PIC  X(02).
     03  VLD1-F02          PIC  9(03).
     03  VLD1-F03          PIC  X(02).
     03  VLD1-F04          PIC  X(08).
     03  VLD1-F05          PIC  9(06).
     03  VLD1-F06          PIC  9(01).
     03  VLD1-F07          PIC  X(02).
     03  VLD1-F08          PIC  9(02).
     03  VLD1-F09          PIC  9(02).
     03  VLD1-F10          PIC  9(04).
     03  VLD1-F11          PIC  9(08).
     03  VLD1-F12          PIC  9(04).
     03  VLD1-F13          PIC  9(08).
     03  FILLER            PIC  X(48).
*---<<  実行制御ファイル  >>---*
 FD  JSMJIKF.
     COPY     JSMJIKF   OF        XFDLIB
              JOINING   JIK       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  STATUS-AREA.
     03  VLD1-ST             PIC  X(02).
     03  JIK-ST              PIC  X(02).
*フラグワーク
 01  FLG-AREA.
     03  END-FLG             PIC  X(03)  VALUE SPACE.
     03  WK-KBN              PIC  X(01)  VALUE SPACE.
     03  JSMJIKF-INV-FLG     PIC  X(03)  VALUE SPACE.
     03  X                   PIC  9(07)  VALUE ZERO.
     03  WK-CNT              PIC  9(07)  VALUE ZERO.
*---<<  日付・時間ワーク  追加  96/07/29  >>---*
 01  SYS-DATE                PIC  9(06)  VALUE ZERO.
 01  HEN-DATE                PIC  9(08)  VALUE ZERO.
 01  SYS-TIME                PIC  9(08)  VALUE ZERO.
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  VLD1-ERR            PIC N(15) VALUE
                             NC"ＶＬＤＦ５００エラー".
     03  VLD2-ERR            PIC N(15) VALUE
                             NC"ＶＬＤＦ６００エラー".
     03  JIK-ERR             PIC N(15) VALUE
                             NC"実行制御Ｆエラー".
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
*--------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                     *
*--------------------------------------------------------------*
 PROCEDURE              DIVISION.
**
 DECLARATIVES.
 VLD1-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE VLD500.
     MOVE        VLD1-ST     TO        E-ST.
     MOVE        "VLD500"    TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     VLD1-ERR    UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 JIK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JSMJIKF.
     MOVE        JIK-ST      TO        E-ST.
     MOVE        "JSMJIKF"   TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     JIK-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
**
 END     DECLARATIVES.
****************************************************************
 PROCESS-START               SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC  UNTIL   END-FLG  =  "END".
     PERFORM       END-SEC.
     STOP      RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*      _０　　初期処理                                        *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     VLD500.
*システム日付／時刻取得
     ACCEPT   SYS-DATE          FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   HEN-DATE.
     ACCEPT    SYS-TIME         FROM   TIME.
*伝票更新開始メッセージ出力
     DISPLAY NC"＃＃売上更新処理開始＃＃" UPON CONS.
*伝票更新フラグセット
     MOVE     "D"           TO   WK-KBN.
     PERFORM  JSMJIKF-ON-SEC.
*
 INIT-END.
     EXIT.
****************************************************************
*      _０　　メイン処理                                      *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE     "MAIN-SEC"          TO   S-NAME.
*ＶＬＤ5００読込み
     PERFORM   VLD500-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
*
     MOVE     "END-SEC"           TO   S-NAME.
*各ファイルのＣＬＯＳＥ
     CLOSE    VLD500.
*伝票更新フラグ解除
     PERFORM  JSMJIKF-OFF-SEC.
     DISPLAY NC"＃＃売上更新処理終了＃＃" UPON CONS.
*
 END-END.
     EXIT.
****************************************************************
*                ＶＬＤ５００読込み                            *
****************************************************************
 VLD500-READ-SEC           SECTION.
*
     MOVE      "VLD500-READ-SEC"  TO    S-NAME.
*
     MOVE      SPACE              TO    VLD1-REC.
     INITIALIZE                         VLD1-REC.
     MOVE      500                TO    VLD1-F02.
     MOVE      "WT"               TO    VLD1-F03.
     MOVE      100                TO    VLD1-F10.
     DISPLAY NC"ＲＥＡＤ　待ち中" UPON CONS.
     READ      VLD500.
     EVALUATE  VLD1-F08
         WHEN  0        DISPLAY NC"ＲＥＡＤ　ＯＫ！"  UPON CONS
         WHEN  01       MOVE  "END"  TO  END-FLG
                        GO           TO  VLD500-READ-EXIT
         WHEN  43       MOVE  "END"  TO  END-FLG
                        GO           TO  VLD500-READ-EXIT

             DISPLAY NC"ＶＬＤ５００ＲＥＡＤエラー" UPON CONS
             DISPLAY NC"入力通知コード　＝　" VLD1-F08 UPON CONS
             MOVE     "END"          TO  END-FLG
     END-EVALUATE.
*受信日付／時間／取引先がＡＬＬ”９”の時、処理終了
     IF      VLD1-F11  =  99999999
     AND     VLD1-F12  =  9999
     AND     VLD1-F13  =  99999999
             MOVE     "END"     TO    END-FLG
             GO                 TO    VLD500-READ-EXIT
     ELSE
             MOVE     "U"       TO    WK-KBN
             PERFORM JSMJIKF-ON-SEC
             CALL "DENPYOUP" USING VLD1-F11 VLD1-F12 VLD1-F13
     END-IF.
 VLD010.
*伝票更新終了待ち合わせ
     PERFORM JSMJIKF-READ-SEC.
     IF      JSMJIKF-INV-FLG  =  "INV"
             MOVE    ZERO     TO      WK-CNT
*************DISPLAY NC"＆＆伝票更新待ち合わせ中＆＆" UPON CONS
             PERFORM VARYING X FROM 1 BY 1 UNTIL X > 100000
                     ADD     1        TO   WK-CNT
             END-PERFORM
             GO      TO               VLD010
     END-IF.
*
 VLD500-READ-EXIT.
     EXIT.
****************************************************************
*                伝票更新フラグセット                         *
****************************************************************
 JSMJIKF-ON-SEC            SECTION.
*
     MOVE      "JSMJIKF-ON-SEC"      TO S-NAME.
     DISPLAY NC"＃＃伝票更新監視開始＃＃" UPON CONS.
*ファイルのＯＰＥＮ
     OPEN      I-O     JSMJIKF.
*実行制御マスタ（伝票更新キーセット）
     MOVE      WK-KBN             TO   JIK-F01.
*実行制御マスタ読込み
     READ    JSMJIKF
             INVALID
               MOVE    "INV"      TO   JSMJIKF-INV-FLG
             NOT  INVALID
               MOVE    SPACE      TO   JSMJIKF-INV-FLG
     END-READ.
***  更新判断
     IF        JSMJIKF-INV-FLG  =  "INV"
               MOVE  SPACE        TO   JIK-REC
               INITIALIZE              JIK-REC
               MOVE  WK-KBN       TO   JIK-F01
               MOVE  NC"売上確認" TO   JIK-F02
               MOVE  1            TO   JIK-F03
               WRITE JIK-REC
     ELSE
               MOVE  1            TO   JIK-F03
               REWRITE JIK-REC
     END-IF.
***  ファイルのクローズ
     CLOSE     JSMJIKF.
*
 JSMJIKF-ON-EXIT.
     EXIT.
****************************************************************
*                伝票更新フラグオフ                           *
****************************************************************
 JSMJIKF-OFF-SEC           SECTION.
*
     MOVE      "JSMJIKF-OFF-SEC"     TO S-NAME.
     DISPLAY NC"＃＃伝票更新監視終了＃＃" UPON CONS.
*ファイルのＯＰＥＮ
     OPEN      I-O     JSMJIKF.
*実行制御マスタ（伝票更新キーセット）
     MOVE     "D"                 TO   JIK-F01.
*実行制御マスタ読込み
     READ    JSMJIKF
             INVALID
               MOVE    "INV"      TO   JSMJIKF-INV-FLG
             NOT  INVALID
               MOVE    SPACE      TO   JSMJIKF-INV-FLG
     END-READ.
***  更新判断
     IF        JSMJIKF-INV-FLG  =  "INV"
               CONTINUE
     ELSE
               MOVE  0            TO   JIK-F03
               REWRITE JIK-REC
     END-IF.
***  ファイルのクローズ
     CLOSE     JSMJIKF.
*
 JSMJIKF-OFF-EXIT.
     EXIT.
****************************************************************
*                売上更新確認                                 *
****************************************************************
 JSMJIKF-READ-SEC          SECTION.
*
     MOVE      "JSMJIKF-READ-SEC"    TO S-NAME.
     OPEN      INPUT   JSMJIKF.
*実行制御マスタ（伝票更新キーセット）
     MOVE     "U"                 TO   JIK-F01.
*実行制御マスタ読込み
     READ    JSMJIKF
             INVALID
             DISPLAY NC"＃売上更新待ち合わせエラー＃" UPON CONS
             DISPLAY NC"＃待ち合わせ読込みエラー　＃" UPON CONS
             STOP  RUN
     END-READ.
*実行判定
     EVALUATE  JIK-F03
         WHEN  0     MOVE  SPACE   TO     JSMJIKF-INV-FLG
         WHEN  1     MOVE  "INV"   TO     JSMJIKF-INV-FLG
         WHEN  OTHER
             DISPLAY NC"＃売上更新待ち合わせエラー＃" UPON CONS
             DISPLAY NC"＃待ち合わせ読込みエラー　＃" UPON CONS
             STOP  RUN
     END-EVALUATE.
***  ファイルのクローズ
     CLOSE     JSMJIKF.
*
 JSMJIKF-READ-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```

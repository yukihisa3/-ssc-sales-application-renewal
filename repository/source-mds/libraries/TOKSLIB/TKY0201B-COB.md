# TKY0201B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/TKY0201B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　受配信管理システム　　　　　　　　*
*    モジュール名　　　　：　在庫引当／売上更新（エラー計上）  *
*    作成日／更新日　　　：　1999/11/15                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　１．取引先Ｍ存在チェック          *
*                            ２．店舗Ｍ存在チェック            *
*                            ３．商品名称Ｍ存在チェック        *
*                            ４．商品変換テーブル存在チェック  *
*                            ５．エラー時、エラーリスト出力    *
*                            ６．商品在庫Ｍ引当済数量更新      *
*                            ７．売上伝票Ｆ更新                *
*                            ８．実行制御Ｆ更新                *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            TKY0201B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          99/11/15.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       K-150SI.
 OBJECT-COMPUTER.       K-150SI.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT
     YA          IS     PITCH-2
     YB          IS     PITCH-15
     YB-21       IS     BAIKAKU.
*--------------------------------------------------------------*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  特販部売上伝票ファイル  >>---*
     SELECT   SHTDENF   ASSIGN              DA-01-VI-SHTDENLD
                        ORGANIZATION        IS    INDEXED
                        ACCESS    MODE      IS    SEQUENTIAL
                        RECORD    KEY       IS    DEN-F55  DEN-F46
                                                  DEN-F47  DEN-F01
                        FILE      STATUS    IS    DEN-ST.
*---<<  商品在庫マスタ  >>---*
     SELECT   ZAMZAIF   ASSIGN    TO        DA-01-VI-ZAMZAIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   ZAI-F01
                                                 ZAI-F02
                                                 ZAI-F03
                        FILE      STATUS    IS   ZAI-ST.
*---<<  取引先マスタ  >>---*
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE      STATUS    IS   TOK-ST.
*---<<  店舗マスタ  >>---*
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TEN-F52 TEN-F011
                        FILE      STATUS    IS   TEN-ST.
*---<<  商品コード変換テーブル  >>---*
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHO-F01
                                                 SHO-F02
                        FILE      STATUS    IS   SHO-ST.
*---<<  商品名称マスタ  >>---*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F01
                        FILE      STATUS    IS   MEI-ST.
*---<<  プリンタ  >>---*
     SELECT   PRTF     ASSIGN               LP-04-PRTF
                       FILE      STATUS     IS   PRT-ST.
*--------------------------------------------------------------*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  ＯＵＴファイル  >>---*
 FD  SHTDENF.
     COPY        SHTDENF   OF        XFDLIB
                 JOINING   DEN       PREFIX.
*---<<  商品在庫マスタ  >>---*
 FD  ZAMZAIF.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZAI       PREFIX.
*---<<  取引先マスタ  >>---*
 FD  HTOKMS.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*---<<  店舗マスタ  >>---*
 FD  HTENMS.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*---<<  商品変換テーブル  >>---*
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
*---<<  商品名称マスタ  >>---*
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*---<<  プリンタ >>---*
 FD  PRTF
     LABEL    RECORD     IS        OMITTED
     LINAGE              IS        66.
 01  PRT-REC            PIC  X(200).

*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  STATUS-AREA.
     03  DEN-ST              PIC  X(02).
     03  ZAI-ST              PIC  X(02).
     03  TOK-ST              PIC  X(02).
     03  TEN-ST              PIC  X(02).
     03  SHO-ST              PIC  X(02).
     03  MEI-ST              PIC  X(02).
     03  PRT-ST              PIC  X(02).
*頁カウント
 01  WK-PAGE                 PIC  9(03)  VALUE ZERO.
*フラグワーク
 01  FLG-AREA.
     03  END-FLG             PIC  X(03)  VALUE SPACE.
     03  KEP-FLG             PIC  X(01)  VALUE SPACE.
     03  CHK-FLG             PIC  X(03)  VALUE SPACE.
     03  ERR-FLG             PIC  X(03)  VALUE SPACE.
     03  HTOKMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     03  HTENMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     03  HSHOTBL-INV-FLG     PIC  X(03)  VALUE SPACE.
     03  HMEIMS-INV-FLG      PIC  X(03)  VALUE SPACE.
     03  SHTDENF-INV-FLG     PIC  X(03)  VALUE SPACE.
     03  JHMJIKF-INV-FLG     PIC  X(03)  VALUE SPACE.
*カウンタークリア
 01  CNT-AREA.
     03  READ-CNT            PIC  9(07)  VALUE ZERO.
     03  OK-CNT              PIC  9(07)  VALUE ZERO.
     03  ERR-CNT             PIC  9(07)  VALUE ZERO.
*キー退避
 01  KEY-AREA.
     03  WRK-BTDATE          PIC  9(08)  VALUE ZERO.
     03  WRK-BTTIME          PIC  9(04)  VALUE ZERO.
     03  WRK-BTTORI          PIC  9(08)  VALUE ZERO.
*計算領域
 01  WRK-AREA.
     03  WRK-HIK             PIC S9(09)V9(02)  VALUE ZERO.
     03  WRK-ZAI             PIC S9(09)V9(02)  VALUE ZERO.
*---<<  日付・時間ワーク  追加  96/07/29  >>---*
 01  SYS-DATE                PIC  9(06)  VALUE ZERO.
 01  HEN-DATE                PIC  9(08)  VALUE ZERO.
 01  SYS-TIME                PIC  9(08)  VALUE ZERO.
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  HEN-ERR           PIC N(15) VALUE
                        NC"変換売上伝票ＤＴエラー".
     03  DEN-ERR           PIC N(15) VALUE
                        NC"売上伝票ＤＴエラー".
     03  ZAI-ERR           PIC N(15) VALUE
                        NC"商品在庫Ｍエラー".
     03  TOK-ERR           PIC N(15) VALUE
                        NC"取引先Ｍエラー".
     03  TEN-ERR           PIC N(15) VALUE
                        NC"店舗Ｍエラー".
     03  SHO-ERR           PIC N(15) VALUE
                        NC"商品変換Ｔエラー".
     03  MEI-ERR           PIC N(15) VALUE
                        NC"商品名称Ｍエラー".
     03  PRT-ERR           PIC N(15) VALUE
                        NC"プリンタＦエラー".
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
*見出し*
 01  LIST-M1.
     03  FILLER  CHARACTER TYPE      BAIKAKU.
         05  FILLER        PIC X(41) VALUE     SPACE.
         05  FILLER        PIC N(20) VALUE
           NC"【エラーデータ変換リスト】（再チェック）".
         05  FILLER        PIC X(08) VALUE     SPACE.
     03  FILLER  CHARACTER TYPE      PITCH-2.
         05  FILLER        PIC N(03) VALUE
           NC"処理日".
         05  FILLER        PIC X(01) VALUE
             ":".
         05  LSYS-YY       PIC 9(02).
         05  FILLER        PIC X(01) VALUE
             "/".
         05  LSYS-MM       PIC Z9.
         05  FILLER        PIC X(01) VALUE
             "/".
         05  LSYS-DD       PIC Z9.
         05  FILLER        PIC X(02) VALUE     SPACE.
         05  FILLER        PIC N(01) VALUE
           NC"頁".
         05  FILLER        PIC X(01) VALUE
             ":".
         05  LPAGE         PIC ZZ9.
 01  LIST-M2.
     03  FILLER  CHARACTER TYPE      PITCH-2.
         05  FILLER        PIC X(02) VALUE     SPACE.
         05  FILLER        PIC N(03) VALUE
           NC"伝票_".
         05  FILLER        PIC X(04) VALUE     SPACE.
         05  FILLER        PIC N(03) VALUE
           NC"取引先".
         05  FILLER        PIC X(02) VALUE
             "CD".
         05  FILLER        PIC X(01) VALUE     SPACE.
     03  FILLER  CHARACTER TYPE      PITCH-15.
         05  FILLER        PIC N(02) VALUE
           NC"店舗".
         05  FILLER        PIC X(02) VALUE
             "CD".
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  FILLER        PIC N(03) VALUE
           NC"店舗名".
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  FILLER        PIC N(05) VALUE
           NC"取引先商品".
         05  FILLER        PIC X(02) VALUE
             "CD".
         05  FILLER        PIC X(05) VALUE     SPACE.
     03  FILLER  CHARACTER TYPE      PITCH-2.
         05  FILLER        PIC N(03) VALUE
           NC"商品名".
         05  FILLER        PIC X(17) VALUE     SPACE.
         05  FILLER        PIC N(04) VALUE
           NC"Ｄ原単価".
         05  FILLER        PIC X(05) VALUE     SPACE.
         05  FILLER        PIC N(04) VALUE
           NC"Ｍ原単価".
         05  FILLER        PIC X(05) VALUE     SPACE.
         05  FILLER        PIC N(04) VALUE
           NC"Ｄ売単価".
         05  FILLER        PIC X(05) VALUE     SPACE.
         05  FILLER        PIC N(04) VALUE
           NC"Ｍ売単価".
         05  FILLER        PIC X(02) VALUE     SPACE.
         05  FILLER        PIC N(05) VALUE
           NC"エラー内容".
 01  LIST-M3.
     03  FILLER            PIC X(02) VALUE     SPACE.
     03  FILLER            PIC X(13) VALUE     "## ﾊﾞｯﾁNO. : ".
     03  L-BTDATE          PIC 9(08).
     03  FILLER            PIC X(03) VALUE     " - ".
     03  L-BTTIME          PIC 9(04).
     03  FILLER            PIC X(03) VALUE     " - ".
     03  L-BTTORI          PIC 9(08).
     03  FILLER            PIC X(03) VALUE     " ##".
 01  LIST-SEN.
     03  LIST-SEN-OCC      OCCURS    136.
         05  FILLER        PIC X(01) VALUE     "-".
*明細行*
 01  LIST-D.
     03  FILLER  CHARACTER TYPE      PITCH-15.
         05  FILLER        PIC X(02) VALUE     SPACE.
         05  L-DEN         PIC 9(09).
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-TOR         PIC 9(08).
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-TENCD       PIC 9(05).
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-TENNM       PIC X(05).
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-SHOCD       PIC X(13).
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-SHONM       PIC X(18).
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-DGEN        PIC Z,ZZZ,ZZ9.99.
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-MGEN        PIC Z,ZZZ,ZZ9.99.
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-DURI        PIC Z,ZZZ,ZZ9.99.
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-MURI        PIC Z,ZZZ,ZZ9.99.
         05  FILLER        PIC X(01) VALUE     SPACE.
         05  L-ERR         PIC N(10).
 01  DUMMY                 PIC X(01) VALUE     SPACE.
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
 DEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SHTDENF.
     MOVE        DEN-ST      TO        E-ST.
     MOVE        "SHTDENF"   TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     DEN-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 ZAI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE ZAMZAIF.
     MOVE        ZAI-ST      TO        E-ST.
     MOVE        "ZAMZAIF"   TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     ZAI-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 TOK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HTOKMS.
     MOVE        TOK-ST      TO        E-ST.
     MOVE        "HTOKMS"    TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     TOK-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 TEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HTENMS.
     MOVE        TEN-ST      TO        E-ST.
     MOVE        "HTENMS"    TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     TEN-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 SHO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HSHOTBL.
     MOVE        SHO-ST      TO        E-ST.
     MOVE        "HSHOTBL"   TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     SHO-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HMEIMS.
     MOVE        MEI-ST      TO        E-ST.
     MOVE        "HMEIMS"    TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     MEI-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
 PRT-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE PRTF.
     MOVE        PRT-ST      TO        E-ST.
     MOVE        "PRTF   "   TO        E-FILE.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     ERR-FILE    UPON      CONS.
     DISPLAY     ERR-NAME    UPON      CONS.
     DISPLAY     PRT-ERR     UPON      CONS.
     MOVE        "4000"      TO        PROGRAM-STATUS.
     STOP        RUN.
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
     OPEN     I-O       ZAMZAIF  SHTDENF.
     OPEN     INPUT     HTOKMS   HTENMS   HSHOTBL   HMEIMS.
     OPEN     OUTPUT    PRTF.
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
*明細行初期化
     MOVE      SPACE              TO   LIST-D.
     MOVE      ZERO               TO   CNT-AREA.
*END-FLG CLEAR
     MOVE      SPACE         TO     END-FLG.
*変換後売上伝票ファイルスタート
     MOVE      1             TO     DEN-F55.
     MOVE      ZERO          TO     DEN-F46.
     MOVE      ZERO          TO     DEN-F47.
     MOVE      ZERO          TO     DEN-F01.
     START     SHTDENF  KEY  IS  >=   DEN-F55 DEN-F46 DEN-F47
                                      DEN-F01
      INVALID
         MOVE     "END"      TO     END-FLG
         DISPLAY NC"＃＃エラーＤＴ無し＃＃" UPON CONS
      NOT  INVALID
         PERFORM   SHTDENF-READ-SEC
         IF   END-FLG = "END"
              DISPLAY NC"＃＃エラーＤＴ無し＃＃" UPON CONS
         END-IF
     END-START.
*
 INIT-END.
     EXIT.
****************************************************************
*      _０　　メイン処理                                      *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE     "MAIN-SEC"          TO   S-NAME.
*売上エラーチェック
     PERFORM   SHTDENF-CHK-SEC.
*売上データ読込み
     PERFORM   SHTDENF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*      2.1.1　売上伝票ファイルエラーチェック                   *
****************************************************************
 SHTDENF-CHK-SEC        SECTION.
*
     MOVE     "SHTDENF-CHK-SEC"   TO   S-NAME.
*フラグ初期化
     INITIALIZE         KEP-FLG  ERR-FLG  CHK-FLG.
*変換後売上伝票Ｆエラーチェック
     PERFORM   ERR-CHK-SEC.
*在庫引当処理
*## 2002/11/19 引当済の場合は，在庫更新をしない ##
     IF  HSHOTBL-INV-FLG  =  SPACE
     AND DEN-F262         =  ZERO
         PERFORM  ZAIKO-SEC
         IF  KEP-FLG  =  SPACE
             MOVE     1       TO     DEN-F27D
         END-IF
         MOVE         1       TO     DEN-F262
     END-IF.
*エラー時、エラー区分セット
     IF  ERR-FLG  =  "ERR"
         MOVE         1       TO     DEN-F55
         ADD          1       TO     ERR-CNT
     ELSE
         MOVE         0       TO     DEN-F55
         ADD          1       TO     OK-CNT
     END-IF.
*更新済みＦＬＧセット
     MOVE          1          TO     DEN-F54.
*売上伝票ファイル更新
     REWRITE       DEN-REC.
*
 SHTDENF-CHK-EXIT.
     EXIT.
****************************************************************
*      2.1.1　売上伝票ファイル作成                            *
****************************************************************
 ERR-CHK-SEC            SECTION.
*
     MOVE     "ERR-CHK-SEC"       TO   S-NAME.
*取引先マスタ存在チェック*
     PERFORM     HTOKMS-READ-SEC.
     IF  HTOKMS-INV-FLG  =  "INV"
         MOVE NC"取引先マスタ　未登録" TO      L-ERR
         PERFORM           ERR-EDT-SEC
         PERFORM           ERR-WRT-SEC
         MOVE    "ERR"     TO        ERR-FLG
     ELSE
         MOVE    TOK-F52   TO        DEN-F24
         MOVE    TOK-F86   TO        DEN-F279  DEN-F27A
     END-IF.
*店舗マスタ存在チェック*
     PERFORM     HTENMS-READ-SEC.
     IF  HTENMS-INV-FLG  =  "INV"
         MOVE NC"店舗　マスタ　未登録" TO      L-ERR
         PERFORM       ERR-EDT-SEC
         PERFORM       ERR-WRT-SEC
         MOVE    "ERR"     TO        ERR-FLG
     ELSE
         MOVE    99        TO        DEN-F06
     END-IF.
*商品変換ＴＢＬチェック*
     PERFORM     HSHOTBL-READ-SEC.
     IF  HSHOTBL-INV-FLG  =  "INV"
         MOVE NC"商品変換ＴＢＬ未登録" TO      L-ERR
         PERFORM       ERR-EDT-SEC
         PERFORM       ERR-WRT-SEC
         MOVE    "ERR"     TO        ERR-FLG
     ELSE
*        *商品名称マスタチェック*
         MOVE    SHO-F04   TO        DEN-F08   DEN-F09
         IF      DEN-F01 = 173 AND DEN-XI0004(40:6) = "001460"
                 MOVE "86"    TO     DEN-F08   DEN-F09
         END-IF
         IF      DEN-F01 = 173 AND DEN-XI0004(40:6) = "001473"
                 MOVE "84"    TO     DEN-F08   DEN-F09
         END-IF
         IF      DEN-F01 = 173 AND DEN-XI0004(40:6) = "001647"
                 MOVE "7S"    TO     DEN-F08   DEN-F09
         END-IF
         MOVE    SHO-F031  TO        DEN-F1411 MEI-F011
         MOVE    SHO-F032  TO        DEN-F1412 MEI-F012
         PERFORM  HMEIMS-READ-SEC
         IF  HMEIMS-INV-FLG  =  "INV"
             MOVE NC"商品名称マスタ未登録" TO  L-ERR
             PERFORM   ERR-EDT-SEC
             PERFORM   ERR-WRT-SEC
             MOVE      "ERR"         TO        ERR-FLG
         ELSE
             IF    DEN-F1421  =  SPACE
                   MOVE MEI-F031     TO        DEN-F1421
                   MOVE MEI-F032     TO        DEN-F1422
             END-IF
             MOVE          MEI-F041  TO        DEN-F171
         END-IF
*        *単価チェック*
         IF  (DEN-F172 NOT = SHO-F05) OR
             (DEN-F173 NOT = SHO-F06)
             MOVE NC"単価が違います！　　" TO  L-ERR
             IF  (DEN-F172 NOT = SHO-F05)
                 MOVE      SHO-F05   TO        L-MGEN
             END-IF
             IF  (DEN-F173 NOT = SHO-F06)
                 MOVE      SHO-F06   TO        L-MURI
             END-IF
             PERFORM   ERR-EDT-SEC
             PERFORM   ERR-WRT-SEC
         END-IF
     END-IF.
*
 ERR-CHK-EXIT.
     EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-SEC              SECTION.
*
     MOVE     "ZAIKO-SEC"         TO   S-NAME.
*商品在庫マスタ存在チェック
     MOVE    DEN-F08         TO   ZAI-F01.
     MOVE    DEN-F1411       TO   ZAI-F021.
     MOVE    DEN-F1412       TO   ZAI-F022.
     MOVE    SHO-F08         TO   ZAI-F03.
     READ    ZAMZAIF
             INVALID
             PERFORM   ZAIKO-UPDATE1-SEC
             NOT  INVALID
             PERFORM   ZAIKO-UPDATE2-SEC
     END-READ.
*
 ZAIKO-EXIT.
     EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-UPDATE1-SEC      SECTION.
*
     MOVE     "ZAIKO-UPDATE1-SEC" TO   S-NAME.
*商品在庫Ｍが未存在の為、在庫マスタ作成
      MOVE      "1"           TO   KEP-FLG.
*商品在庫マスタ初期化
      MOVE      SPACE         TO   ZAI-REC.
      INITIALIZE                   ZAI-REC.
*商品在庫マスタ項目セット
      MOVE      DEN-F08       TO   ZAI-F01.
      MOVE      DEN-F1411     TO   ZAI-F021.
      MOVE      DEN-F1412     TO   ZAI-F022.
      MOVE      SHO-F08       TO   ZAI-F03.
*未出庫数＝未出庫数＋数量
      COMPUTE   ZAI-F27       =    ZAI-F27  +  DEN-F15.
*商品名称マスタ読込み
      PERFORM   HMEIMS-READ-SEC.
*商品名称マスタ存在チェック
      IF  HMEIMS-INV-FLG  =  SPACE
          MOVE  MEI-F031      TO   ZAI-F30
          MOVE  HEN-DATE      TO   ZAI-F98
          MOVE  HEN-DATE      TO   ZAI-F99
          WRITE ZAI-REC
      END-IF.
*
 ZAIKO-UPDATE1-EXIT.
      EXIT.
****************************************************************
*      2.2     在庫引当                                        *
****************************************************************
 ZAIKO-UPDATE2-SEC      SECTION.
*
     MOVE     "ZAIKO-UPDATE2-SEC" TO   S-NAME.
*引当後在庫数チェック
*    現在庫数－引当済数＝引当可能在庫数
     COMPUTE   WRK-ZAI   =   ZAI-F04  -  ZAI-F28.
*    引当可能在庫数－発注数量＝引当後在庫数
     COMPUTE   WRK-HIK   =   WRK-ZAI  -  DEN-F15.
     IF  WRK-HIK  <  0
         MOVE      "1"      TO   KEP-FLG
*        未出庫数に数量加算
         COMPUTE  ZAI-F27   =   ZAI-F27  +  DEN-F15
         MOVE     HEN-DATE  TO  ZAI-F99
*        商品在庫マスタ更新
         REWRITE  ZAI-REC
     ELSE
*        引当済数に数量加算
         COMPUTE  ZAI-F28   =   ZAI-F28  +  DEN-F15
*        未出庫数に数量加算
         COMPUTE  ZAI-F27   =   ZAI-F27  +  DEN-F15
         MOVE     HEN-DATE  TO  ZAI-F99
*        商品在庫マスタ更新
         REWRITE  ZAI-REC
     END-IF.
*
 ZAIKO-UPDATE2-EXIT.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
*
     MOVE     "END-SEC"           TO   S-NAME.
*各ファイルのＣＬＯＳＥ
     CLOSE    SHTDENF ZAMZAIF HTOKMS HTENMS HSHOTBL HMEIMS PRTF.
     DISPLAY "## ERROR READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "## ERROR OK   CNT = " OK-CNT    UPON CONS.
     DISPLAY "## ERROR NG   CNT = " ERR-CNT   UPON CONS.
     DISPLAY NC"＃＃エラーＤＴ復旧処理完了＃＃" UPON CONS.
*
 END-END.
     EXIT.
****************************************************************
*                  エラーリスト明細編集　　　                  *
****************************************************************
 ERR-EDT-SEC               SECTION.
*
     MOVE   "ERR-EDT-SEC"  TO        S-NAME.
     MOVE        DEN-F02   TO        L-DEN.
     MOVE        DEN-F01   TO        L-TOR.
     MOVE        DEN-F07   TO        L-TENCD.
     MOVE        DEN-F30   TO        L-TENNM.
     MOVE        DEN-F25   TO        L-SHOCD.
     MOVE        DEN-F1421 TO        L-SHONM.
     MOVE        DEN-F1422(1:3) TO   L-SHONM(16:3).
     MOVE        DEN-F172  TO        L-DGEN.
     MOVE        DEN-F173  TO        L-DURI.
 ERR-EDT-EXIT.
     EXIT.
****************************************************************
*                  エラーリスト出力　　　　　                  *
****************************************************************
 ERR-WRT-SEC               SECTION.
*
     MOVE   "ERR-WRT-SEC"  TO        S-NAME.
     IF   WK-PAGE = ZERO
     OR   LINAGE-COUNTER > 61
     OR   WRK-BTDATE  NOT =  DEN-F46
     OR   WRK-BTTIME  NOT =  DEN-F47
     OR   WRK-BTTORI  NOT =  DEN-F01
         IF  WK-PAGE  >  ZERO
             MOVE          SPACE     TO        PRT-REC
             WRITE         PRT-REC   AFTER     PAGE
         END-IF
         ADD     1         TO        WK-PAGE
         MOVE    WK-PAGE   TO        LPAGE
         MOVE    HEN-DATE(1:4)  TO        LSYS-YY
         MOVE    HEN-DATE(5:2)  TO        LSYS-MM
         MOVE    HEN-DATE(7:2)  TO        LSYS-DD
         MOVE    DEN-F46        TO        L-BTDATE
         MOVE    DEN-F47        TO        L-BTTIME
         MOVE    DEN-F01        TO        L-BTTORI
         WRITE   PRT-REC   FROM      LIST-M1   AFTER     1
         WRITE   PRT-REC   FROM      LIST-M3   AFTER     2
         WRITE   PRT-REC   FROM      LIST-SEN  AFTER     1
         WRITE   PRT-REC   FROM      LIST-M2   AFTER     1
         WRITE   PRT-REC   FROM      LIST-SEN  AFTER     1
*キーの入れ替え
         MOVE    DEN-F46        TO        WRK-BTDATE
         MOVE    DEN-F47        TO        WRK-BTTIME
         MOVE    DEN-F01        TO        WRK-BTTORI
     END-IF.
     WRITE       PRT-REC   FROM      LIST-D    AFTER     1.
     MOVE        SPACE     TO        LIST-D.
 ERR-WRT-EXIT.
     EXIT.
****************************************************************
*                得意先マスタ読込み                            *
****************************************************************
 HTOKMS-READ-SEC           SECTION.
*
     MOVE      "HTOKMS-READ-SEC"  TO    S-NAME.
*
     MOVE      DEN-F01     TO     TOK-F01.
     READ      HTOKMS
               INVALID
               MOVE      "INV"    TO    HTOKMS-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*                店舗マスタ読込み                              *
****************************************************************
 HTENMS-READ-SEC           SECTION.
*
     MOVE      "HTENMS-READ-SEC"  TO    S-NAME.
*
     MOVE      DEN-F01     TO     TEN-F52.
     MOVE      DEN-F07     TO     TEN-F011.
     READ      HTENMS
               INVALID
               MOVE      "INV"    TO    HTENMS-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HTENMS-INV-FLG
     END-READ.
*
 HTENMS-READ-EXIT.
     EXIT.
****************************************************************
*                商品変換テーブル読込み                        *
****************************************************************
 HSHOTBL-READ-SEC          SECTION.
*
     MOVE      "HSHOTBL-READ-SEC" TO    S-NAME.
*
     MOVE      DEN-F01     TO     SHO-F01.
     MOVE      DEN-F25     TO     SHO-F02.
     READ      HSHOTBL
               INVALID
               MOVE      "INV"    TO    HSHOTBL-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HSHOTBL-INV-FLG
     END-READ.
*
 HSHOTBL-READ-EXIT.
     EXIT.
****************************************************************
*                商品名称マスタ読込み                          *
****************************************************************
 HMEIMS-READ-SEC           SECTION.
*
     MOVE      "HMEIMS-READ-SEC"  TO    S-NAME.
*
     MOVE      SHO-F031    TO     MEI-F011.
     MOVE      SHO-F032    TO     MEI-F012.
     READ      HMEIMS
               INVALID
               MOVE      "INV"    TO    HMEIMS-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
****************************************************************
*                売上伝票ファイル読込み                        *
****************************************************************
 SHTDENF-READ-SEC          SECTION.
*
     MOVE      "SHTDENF-READ-SEC" TO    S-NAME.
*
     READ      SHTDENF   AT  END
               MOVE      "END"    TO    END-FLG
               NOT  AT  END
               ADD        1       TO    READ-CNT
     END-READ.
*
     IF        READ-CNT(5:3)  =  "000"
               DISPLAY "READ-CNT = " READ-CNT UPON CONS
     END-IF.
*
 SHTDENF-READ-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```

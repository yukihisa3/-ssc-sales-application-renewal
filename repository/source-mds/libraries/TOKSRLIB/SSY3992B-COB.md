# SSY3992B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY3992B.COB`

## ソースコード

```cobol
***********************************************************
*    顧客名　　　　　：（株）サカタのタネ殿　　　　       *
*    サブシステム　　：ナフコ新ＥＤＩシステム　　         *
*    業務名　　　　　：ナフコデータ管理                   *
*    モジュール名　　：ナフコ受注／出荷ＤＴ作成　　　　   *
*    作成日／作成者　：2020/05/25    NAV                  *
*    処理概要　　　　：                                   *
*      管理番号を受け取り、基本情報Ｆ／数量訂正Ｆより、　 *
*      ナフコチェックデータを作成する。（受信日Ｖｅｒ)    *
*    更新日／更新者　：                                   *
*    更新内容　　　　：　　　　　　　　　　　　　　　　　 *
***********************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SSY3992B.
 AUTHOR.               ASS.
 DATE-WRITTEN.         2020/05/25.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
* 数量訂正ファイル
     SELECT  NFSUTEF
       ASSIGN         TO  NFSUTEL1
       ORGANIZATION   IS  INDEXED
       ACCESS MODE    IS  RANDOM
       RECORD KEY     IS  ST1-F01   *> 管理番号
                          ST1-F05   *> 作場ＣＤ
                          ST1-F06   *> 店舗ＣＤ
                          ST1-F07   *> 納品場所
                          ST1-F08   *> 店着日
                          ST1-F09   *> 伝票番号
       FILE STATUS    IS  ST1-ST.

* 基本情報ファイル
     SELECT  NFJOHOF
       ASSIGN         TO  NFJOHOL2
       ORGANIZATION   IS  INDEXED
       ACCESS MODE    IS  SEQUENTIAL
       RECORD KEY     IS  KH2-F02   *> 受信日
                          KH2-F03   *> 受信時刻
                          KH2-F04   *> 受信取引先
                          KH2-F05   *> 作場ＣＤ
                          KH2-F06   *> 店舗ＣＤ
                          KH2-F07   *> 伝票番号
                          KH2-F08   *> 行番号
                          KH2-F09   *> 納品日
       FILE STATUS    IS  KH2-ST.

* ナフコ商品マスタ
     SELECT  NFSHOMS
       ASSIGN         TO  NFSHOMS1
       ORGANIZATION   IS  INDEXED
       ACCESS MODE    IS  RANDOM
       RECORD KEY     IS  SYO-F01   *> ナフコ商品ＣＤ
       FILE STATUS    IS  SYO-ST.

* ナフコチェックデータ
     SELECT  NFCKDTF
       ASSIGN         TO  NFCKDTL1
       ORGANIZATION   IS  INDEXED
       ACCESS MODE    IS  RANDOM
       RECORD KEY     IS  CKD-F00   *> 取引先ＣＤ
                          CKD-F01   *> 店舗ＣＤ
                          CKD-F05   *> 店舗ＣＤ
                          CKD-F06   *> 店舗ＣＤ
                          CKD-F03   *> 伝票番号
                          CKD-F04   *> 行番号
                          CKD-F07   *> ナフコ商品ＣＤ
       FILE STATUS    IS  CKD-ST.

*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 数量訂正ファイル                                   *
****************************************************************
 FD  NFSUTEF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFSUTEF   OF   XFDLIB
                       JOINING   ST1       AS   PREFIX.
****************************************************************
*    FILE = 基本情報ファイル                                   *
****************************************************************
 FD  NFJOHOF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFJOHOF   OF   XFDLIB
                       JOINING   KH2       AS   PREFIX.
****************************************************************
*    FILE = ナフコ商品マスタ                                   *
****************************************************************
 FD  NFSHOMS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFSHOMS   OF   XFDLIB
                       JOINING   SYO       AS   PREFIX.
****************************************************************
*    FILE = ナフコチェックデータ                               *
****************************************************************
 FD  NFCKDTF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NFCKDTF   OF   XFDLIB
                       JOINING   CKD       AS   PREFIX.
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  ST1-ST             PIC  X(02).
     03  KH2-ST             PIC  X(02).
     03  SYO-ST             PIC  X(02).
     03  CKD-ST             PIC  X(02).
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER             PIC  X(05)  VALUE " *** ".
     03  S-NAME             PIC  X(30).
***  エラーファイル
 01  FILE-ERR.
     03  ST1-ERR           PIC N(20) VALUE
         NC"数量訂正ファイル１・エラー".
     03  KH2-ERR           PIC  N(20)  VALUE
         NC"基本情報ファイル２・エラー".
     03  SYO-ERR           PIC N(20) VALUE
         NC"ナフコ商品マスタ１・エラー".
     03  CKD-ERR           PIC N(20) VALUE
         NC"ナフコチェックデータ１・エラー".

*読込フラグ領域
 01  FLG-AREA.
     03  END-FLG            PIC  X(03)  VALUE SPACE.
     03  NFCKDTF-INV-FLG    PIC  X(03)  VALUE SPACE.
     03  NFSUTEF-INV-FLG    PIC  X(03)  VALUE SPACE.
     03  NFSHOMS-INV-FLG    PIC  X(03)  VALUE SPACE.
*読込・書込カウント領域
 01  CNT-AREA.
     03  NFJOHOF-RD-CNT   PIC  9(07)  VALUE ZERO.
     03  SKIP-CNT           PIC  9(07)  VALUE ZERO.
     03  NFCKDTF-WT-CNT     PIC  9(07)  VALUE ZERO.
     03  NFCKDTF-RW-CNT     PIC  9(07)  VALUE ZERO.


 01  DATE-AREA.
     03  WK-DATE            PIC  9(06).
     03  SYS-DATE           PIC  9(08).
 01  WK-TIME.
     03  SYS-TIME           PIC  9(06).


*日付変換サブルーチン
 01  SKYDTCKB-AREA.
     03  SKYDTCKB-IN-KBN          PIC  X(01).
     03  SKYDTCKB-IN-YMD6         PIC  9(06).
     03  SKYDTCKB-IN-YMD8         PIC  9(08).
     03  SKYDTCKB-OUT-RET         PIC  X(01).
     03  SKYDTCKB-OUT-YMD         PIC  9(08).
*------------------------------------------------------------*
 LINKAGE              SECTION.
*------------------------------------------------------------*
* 入力パラメータ
 01  PA-JYUSNDT   PIC  9(08). *> 管理番号
*
**************************************************************
 PROCEDURE             DIVISION
                               USING   PA-JYUSNDT.
**************************************************************
 DECLARATIVES.
 ST1-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFSUTEF.
     DISPLAY     ST1-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ST1-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 KH2-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFJOHOF.
     DISPLAY     KH2-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     KH2-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SYO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFSHOMS.
     DISPLAY     SYO-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SYO-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 CKD-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFCKDTF.
     DISPLAY     CKD-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     CKD-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE  "PROCESS-START"  TO  S-NAME.

     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL  END-FLG = "END".
     PERFORM  END-SEC.

     STOP RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               1.0       *
****************************************************************
 INIT-SEC              SECTION.
     MOVE  "INIT-SEC"       TO  S-NAME.

* ファイルのＯＰＥＮ
     OPEN  INPUT  NFSUTEF.
     OPEN  INPUT  NFJOHOF.
     OPEN  INPUT  NFSHOMS.
     OPEN  I-O    NFCKDTF.

* システム日付取得
     ACCEPT  WK-DATE  FROM DATE.
     ACCEPT  WK-TIME  FROM TIME.
     MOVE  "3"              TO  SKYDTCKB-IN-KBN.
     MOVE  WK-DATE          TO  SKYDTCKB-IN-YMD6.
     MOVE  ZERO             TO  SKYDTCKB-IN-YMD8.
     MOVE  ZERO             TO  SKYDTCKB-OUT-RET.
     MOVE  ZERO             TO  SKYDTCKB-OUT-YMD.
     CALL  "SKYDTCKB"  USING SKYDTCKB-IN-KBN
                             SKYDTCKB-IN-YMD6
                             SKYDTCKB-IN-YMD8
                             SKYDTCKB-OUT-RET
                             SKYDTCKB-OUT-YMD.
     MOVE  SKYDTCKB-OUT-YMD TO  SYS-DATE.

* 初期値設定

* 基本情報ファイルの初期読み込み
     MOVE  SPACE            TO  END-FLG.
     MOVE  SPACE            TO  KH2-REC.
     INITIALIZE                 KH2-REC.
     MOVE  PA-JYUSNDT       TO  KH2-F02.  *> 受信日
     START NFJOHOF KEY IS >= KH2-F02  KH2-F03  KH2-F04
                             KH2-F05  KH2-F06
                             KH2-F07  KH2-F08  KH2-F09
           INVALID
           DISPLAY NC"＃対象データ無し（ＳＴ）＃" UPON CONS
           MOVE    4000     TO  PROGRAM-STATUS
           MOVE    "END"    TO  END-FLG
           GO               TO  INIT-EXIT
     END-START.
* 基本情報ファイル読み込み
     PERFORM  NFJOHOF-READ-SEC.
*
     IF   END-FLG  =  "END"
          DISPLAY NC"＃対象データ無し（初Ｒ）＃" UPON CONS
          MOVE    4000     TO  PROGRAM-STATUS
          MOVE    "END"    TO  END-FLG
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*  基本情報ファイル・読込み
****************************************************************
 NFJOHOF-READ-SEC            SECTION.
     MOVE  "NFJOHOF-READ-SEC"  TO  S-NAME.
*
     READ  NFJOHOF
           NEXT  AT  END
           MOVE     "END"      TO  END-FLG
           GO                  TO  NFJOHOF-READ-EXIT
     END-READ.
*
     ADD   1                   TO  NFJOHOF-RD-CNT.
     IF  NFJOHOF-RD-CNT(5:3) = "000" OR  "500"
         DISPLAY "## READ-CNT = " NFJOHOF-RD-CNT " #"
                 UPON CONS
     END-IF.
*
     IF  PA-JYUSNDT  <  KH2-F02
           MOVE     "END"      TO  END-FLG
           GO                  TO  NFJOHOF-READ-EXIT
     END-IF.
*
 NFJOHOF-READ-EXIT.
     EXIT.
****************************************************************
*  数量訂正ファイル読込
****************************************************************
 NFSUTEF-READ-SEC            SECTION.
     MOVE  "NFSYTEF-READ-SEC" TO S-NAME.
*
     MOVE   KH2-F01           TO ST1-F01.
     MOVE   KH2-F05           TO ST1-F05.
     MOVE   KH2-F06           TO ST1-F06.
     MOVE   KH2-HE20          TO ST1-F07.
     MOVE   KH2-F09           TO ST1-F08.
     MOVE   KH2-F07           TO ST1-F09.
*
     READ  NFSUTEF
           INVALID     MOVE  "INV"    TO  NFSUTEF-INV-FLG
           NOT INVALID MOVE  SPACE    TO  NFSUTEF-INV-FLG
     END-READ.
*
 NFSUTEF-READ-EXIT.
     EXIT.
****************************************************************
*  ナフコチェックデータ存在チェック
****************************************************************
 NFCKDTF-READ-SEC            SECTION.
     MOVE  "NFCKDTF-READ-SEC" TO S-NAME.
*
     MOVE   KH2-F04           TO CKD-F00.
     MOVE   KH2-F06           TO CKD-F01.
     MOVE   KH2-HE10          TO CKD-F05.
     MOVE   ZERO              TO CKD-F06.
     MOVE   KH2-F07           TO CKD-F03.
     MOVE   KH2-F08           TO CKD-F04.
     MOVE   KH2-F13           TO CKD-F07.
*
     READ  NFCKDTF
           INVALID     MOVE  "INV"    TO  NFCKDTF-INV-FLG
           NOT INVALID MOVE  SPACE    TO  NFCKDTF-INV-FLG
     END-READ.
*
 NFCKDTF-READ-EXIT.
     EXIT.
****************************************************************
*  メイン処理                                        2.0       *
****************************************************************
 MAIN-SEC               SECTION.
     MOVE  "MAIN-SEC"   TO  S-NAME.
*
     PERFORM  NFSUTEF-READ-SEC.
*
     PERFORM  NFCKDTF-READ-SEC.
*
     IF  NFCKDTF-INV-FLG  =  "INV"
         MOVE   SPACE        TO   CKD-REC
         INITIALIZE               CKD-REC
     END-IF.
*
     MOVE     KH2-F04        TO   CKD-F00.
     MOVE     KH2-F06        TO   CKD-F01.
     MOVE     KH2-HE20       TO   CKD-F02.
     MOVE     KH2-F07        TO   CKD-F03.
     MOVE     KH2-F08        TO   CKD-F04.
     MOVE     KH2-HE10       TO   CKD-F05.
     MOVE     ZERO           TO   CKD-F06.
     MOVE     KH2-F13        TO   CKD-F07.
*商品名取得
*ナフコ商品マスタから項目設定 *******************************
     MOVE     KH2-F13        TO    SYO-F01.
     PERFORM  NFSHOMS-READ-SEC.
     IF   NFSHOMS-INV-FLG = SPACE
          MOVE SYO-F05       TO   CKD-F08
          MOVE SYO-F06       TO   CKD-F09
     ELSE
          MOVE SPACE         TO   CKD-F08
          MOVE SPACE         TO   CKD-F09
     END-IF.
     MOVE     "1"            TO   CKD-F10.
     IF  NFSUTEF-INV-FLG = SPACE
         MOVE "1"            TO   CKD-F11
         MOVE ST1-F16        TO   CKD-F209
         MOVE ST1-F14        TO   CKD-F203
     END-IF.
     MOVE     KH2-F05        TO   CKD-F200.

     MOVE  "3"              TO  SKYDTCKB-IN-KBN.
     MOVE  KH2-HE11         TO  SKYDTCKB-IN-YMD6.
     MOVE  ZERO             TO  SKYDTCKB-IN-YMD8.
     MOVE  ZERO             TO  SKYDTCKB-OUT-RET.
     MOVE  ZERO             TO  SKYDTCKB-OUT-YMD.
     CALL  "SKYDTCKB"  USING    SKYDTCKB-IN-KBN
                                SKYDTCKB-IN-YMD6
                                SKYDTCKB-IN-YMD8
                                SKYDTCKB-OUT-RET
                                SKYDTCKB-OUT-YMD.
     MOVE  SKYDTCKB-OUT-YMD  TO   CKD-F201.
     MOVE     KH2-F09        TO   CKD-F202.
     MOVE     KH2-F42        TO   CKD-F204.
     MOVE     KH2-F19        TO   CKD-F205.
     MOVE     KH2-F20        TO   CKD-F206.
     MOVE     KH2-ME10       TO   CKD-F207.
     MOVE     KH2-ME12       TO   CKD-F208.
     MOVE     KH2-HE10       TO   CKD-F20A.
     MOVE     KH2-ME04       TO   CKD-F20B.
     MOVE     KH2-F01        TO   CKD-F40.
     MOVE     KH2-F02        TO   CKD-F41.
     MOVE     KH2-F03        TO   CKD-F42.
     MOVE     KH2-F04        TO   CKD-F43.
*
     IF   NFCKDTF-INV-FLG  =  "INV"
          ADD    1           TO   NFCKDTF-WT-CNT
          WRITE  CKD-REC
     ELSE
          ADD    1           TO   NFCKDTF-RW-CNT
          REWRITE CKD-REC
     END-IF.
*
     PERFORM  NFJOHOF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*    ナフコ商品マスタ検索
****************************************************************
 NFSHOMS-READ-SEC           SECTION.
     MOVE  "NFSHOMS-READ-SEC" TO S-NAME.
*
     READ  NFSHOMS
       INVALID
         MOVE  "INV"             TO  NFSHOMS-INV-FLG
       NOT INVALID
         MOVE  SPACE             TO  NFSHOMS-INV-FLG
     END-READ.
*
 NFSHOMS-READ-EXIT.
     EXIT.
****************************************************************
*  終了処理                                          3.0       *
****************************************************************
 END-SEC                  SECTION.
     MOVE  "END-SEC"      TO  S-NAME.
* ファイルのＯＰＥＮ
     CLOSE  NFSUTEF.
     CLOSE  NFJOHOF.
     CLOSE  NFSHOMS.
     CLOSE  NFCKDTF.
*
     DISPLAY "## READ-CNT   =  "  NFJOHOF-RD-CNT  UPON CONS.
     DISPLAY "## NFCKDTF WT =  "  NFCKDTF-WT-CNT  UPON CONS.
     DISPLAY "## NFCKDTF RW =  "  NFCKDTF-RW-CNT  UPON CONS.
*
 END-EXIT.
     EXIT.
*****************<<  SSY3992B   END PROGRAM  >>******************

```

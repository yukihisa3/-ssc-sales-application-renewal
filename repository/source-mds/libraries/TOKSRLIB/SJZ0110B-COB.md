# SJZ0110B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SJZ0110B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    業務名　　　　　　　：　受信残管理システム　　　　　　　　
*    モジュール名　　　　：　受注残ファイル作成（明細単位）
*    作成日／更新日　　　：　2018/03/14
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　
*    処理概要　　　　　　：　パラメタにて受け取り、受注残Ｆの
*                            登録、修正、削除を行なう。
****************************************************************
*<履歴>*********************************************************
* XXXX/XX/XX XXXXXXXXX ＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮ
* 2018/03/14 高橋　　　新規作成
*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SJZ0110B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          18/03/13.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       PG6000.
 OBJECT-COMPUTER.       PG6000.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
*--------------------------------------------------------------*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  受注残ファイル  >>---*
     SELECT   JYUZANF   ASSIGN    TO        DA-01-VI-JYUZANL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYU-F03
                                                 JYU-F04
                                                 JYU-F06
                                                 JYU-F07
                                                 JYU-F08
                                                 JYU-F11
                                                 JYU-F05
                        FILE      STATUS    IS   JYU-STATUS.
*
*--------------------------------------------------------------*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  受注残ファイル  >>---*
 FD  JYUZANF.
     COPY     JYUZANF   OF        XFDLIB
              JOINING   JYU       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*
 01  SYS-DATE                PIC  9(08).
 01  STATUS-AREA.
     03  JYU-STATUS          PIC  X(02).
 01  WK-SURYO                PIC  9(10)V99.
 01  PSW-AREA.
     03  END-FLG             PIC  X(03)  VALUE SPACE.
     03  ZAMZAIF-INV-FLG     PIC  X(03)  VALUE SPACE.
     03  JYUZANF-INV-FLG     PIC  X(03)  VALUE SPACE.
     03  HSHOTBL-INV-FLG     PIC  X(03)  VALUE SPACE.
 01  CNT-AREA.
     03  KEP-FLG             PIC  X(01)  VALUE SPACE.
     03  MEI-INV             PIC  9(01)  VALUE ZERO.
     03  IN-CNT              PIC  9(07)  VALUE ZERO.
     03  OUT-CNT1            PIC  9(07)  VALUE ZERO.
     03  OUT-CNT2            PIC  9(07)  VALUE ZERO.
     03  SKIP-CNT1           PIC  9(07)  VALUE ZERO.
     03  SKIP-CNT2           PIC  9(07)  VALUE ZERO.
     03  JYU-CNT             PIC  9(07)  VALUE ZERO.
 01  WRK-AREA.
     03  WRK-TANA            PIC  X(06).
     03  WRK-ZAI             PIC S9(09)V99.
     03  WRK-HIK             PIC S9(09)V99.
 01  MSG-AREA1-1.
     03  MSG-ABEND1.
       05  FILLER            PIC  X(04)  VALUE "### ".
       05  ERR-PG-ID         PIC  X(08)  VALUE "SJZ0110B".
       05  FILLER            PIC  X(10)  VALUE " ABEND ###".
     03  MSG-ABEND2.
       05  FILLER            PIC  X(04)  VALUE "### ".
       05  ERR-FL-ID         PIC  X(08).
       05  FILLER            PIC  X(04)  VALUE " ST-".
       05  ERR-STCD          PIC  X(02).
       05  FILLER            PIC  X(04)  VALUE " ###".
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  SYS-TIME2          PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME2.
     03  SYS-TIMEW      PIC  9(06).
     03  FILLER         PIC  9(02).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
 LINKAGE                   SECTION.
 01  PARA-AREA.
     03  PARA-KOSINKBN         PIC X(01).
     03  PARA-DTKBN            PIC X(01).
     03  PARA-TOKCD            PIC 9(08).
     03  PARA-DENNO            PIC 9(09).
     03  PARA-GYO              PIC 9(02).
     03  PARA-SOUSAI           PIC 9(01).
     03  PARA-DENKU            PIC 9(02).
     03  PARA-TENCD            PIC 9(05).
     03  PARA-SOKCD            PIC X(02).
     03  PARA-HATYU            PIC 9(08).
     03  PARA-NOUHIN           PIC 9(08).
     03  PARA-SYUKA            PIC 9(08).
     03  PARA-SAKATACD         PIC X(08).
     03  PARA-HINTAN1          PIC X(05).
     03  PARA-HINTAN2          PIC X(02).
     03  PARA-HINTAN3          PIC X(01).
     03  PARA-TANABAN          PIC X(06).
     03  PARA-JYUTYUSU         PIC 9(09).
     03  PARA-SYUKASU          PIC 9(09).
     03  PARA-GENKA            PIC 9(09).
     03  PARA-BAIKA            PIC 9(09).
     03  PARA-GENKAKIN         PIC 9(09).
     03  PARA-BAIKAKIN         PIC 9(09).
     03  PARA-AITECD           PIC X(13).
     03  PARA-HIKIATE          PIC X(01).
     03  PARA-UPBUMON          PIC X(04).
     03  PARA-UPTANCD          PIC X(02).
     03  PARA-JDATE            PIC 9(08).
     03  PARA-JTIME            PIC 9(04).
   01  PARA-BUMON              PIC X(04).
   01  PARA-TANCD              PIC X(02).
*--------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                     *
*--------------------------------------------------------------*
 PROCEDURE              DIVISION  USING  PARA-AREA
                                         PARA-BUMON
                                         PARA-TANCD.
**
 DECLARATIVES.
**
 FILEERR-SEC1        SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    JYUZANF.
     MOVE     "JYUZANL1"     TO   ERR-FL-ID.
     MOVE      JYU-STATUS    TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000          TO   PROGRAM-STATUS.
     STOP      RUN.
 END     DECLARATIVES.
****************************************************************
 SJZ0110B-START              SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC.
     PERFORM       END-SEC.
 SJZ0110B-END.
     EXIT     PROGRAM.
****************************************************************
*      1.0 　　初期処理                                        *
****************************************************************
 INIT-SEC               SECTION.
*ファイルＯＰＥＮ
     OPEN     I-O       JYUZANF.
*初期化
     MOVE     SPACE          TO   END-FLG.
     MOVE     SPACE          TO   KEP-FLG.
     MOVE     ZERO           TO   IN-CNT.
     MOVE     ZERO           TO   OUT-CNT1.
     MOVE     ZERO           TO   OUT-CNT2.
*システム日付・時刻の取得
     MOVE     "3"            TO   LINK-IN-KBN.
     ACCEPT   LINK-IN-YMD6   FROM DATE.
     MOVE     ZERO           TO   LINK-IN-YMD8.
     MOVE     ZERO           TO   LINK-OUT-RET.
     MOVE     ZERO           TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD    TO   SYS-DATE.
*
     ACCEPT   SYS-TIME2      FROM TIME.
*
 INIT-END.
     EXIT.
****************************************************************
*      ALL    売上伝票ファイル読込
****************************************************************
 JYUZANF-READ-SEC       SECTION.
*
     MOVE    PARA-TOKCD        TO   JYU-F03.
     MOVE    PARA-DENNO        TO   JYU-F04.
     MOVE    PARA-SOUSAI       TO   JYU-F06.
     MOVE    PARA-DENKU        TO   JYU-F07.
     MOVE    PARA-TENCD        TO   JYU-F08.
     MOVE    PARA-NOUHIN       TO   JYU-F11.
     MOVE    PARA-GYO          TO   JYU-F05.
     READ    JYUZANF
             INVALID
             MOVE  "INV"       TO   JYUZANF-INV-FLG
             NOT  INVALID
             MOVE  SPACE       TO   JYUZANF-INV-FLG
     END-READ.
*
 JYUZANF-READ-EXIT.
     EXIT.
****************************************************************
*      2.0 　　メイン処理                                      *
****************************************************************
 MAIN-SEC               SECTION.
*
*受注残ファイル読込
     PERFORM  JYUZANF-READ-SEC.
*****DISPLAY "PARA-DENNO = "PARA-DENNO UPON CONS.
*****DISPLAY "JYUZANF-INV-FLG = " JYUZANF-INV-FLG UPON CONS.
*****DISPLAY "PARA-KOSINKBN   = " PARA-KOSINKBN   UPON CONS.
*受注残ファイル存在チェック結果判定
     IF  JYUZANF-INV-FLG = "INV"
*********更新区分＝登録、修正の時
         IF  PARA-KOSINKBN  =  "1"  OR  "2"
             PERFORM  JYUZANF-WT-SEC
*************DISPLAY "## FILE-WRITE   ##" UPON CONS
         END-IF
*********更新区分＝削除の時
         IF  PARA-KOSINKBN  =  "3"
             GO       TO      MAIN-END
         END-IF
     ELSE
         IF  PARA-KOSINKBN  =  "1"  OR  "2"
             PERFORM  JYUZANF-REWT-SEC
*************DISPLAY "## FILE-REWRITE ##" UPON CONS
         END-IF
         IF  PARA-KOSINKBN  =  "3"
             PERFORM  JYUZANF-DEL-SEC
*************DISPLAY "## FILE-DELETE  ##" UPON CONS
         END-IF
     END-IF.
*
 MAIN-END.
     EXIT.
****************************************************************
*              受注残ファイル作成
****************************************************************
 JYUZANF-WT-SEC         SECTION.
*受注残ファイル初期化
     MOVE     SPACE     TO    JYU-REC.
     INITIALIZE               JYU-REC.
*存在チェック
     MOVE PARA-TOKCD    TO    JYU-F03.       *>取引先CD
     MOVE PARA-DENNO    TO    JYU-F04.       *>伝票番号
     MOVE PARA-SOUSAI   TO    JYU-F06.       *>相殺区分
     MOVE PARA-DENKU    TO    JYU-F07.       *>伝票区分
     MOVE PARA-TENCD    TO    JYU-F08.       *>店舗CD
     MOVE PARA-NOUHIN   TO    JYU-F11.       *>納品日
     MOVE PARA-GYO      TO    JYU-F05.       *>行番号
     MOVE PARA-SOKCD    TO    JYU-F09.       *>出荷場所
     MOVE PARA-HATYU    TO    JYU-F10.       *>発注日
     MOVE PARA-SYUKA    TO    JYU-F12.       *>出荷日
     MOVE PARA-SAKATACD TO    JYU-F13.       *>サカタ商品CD
     MOVE PARA-HINTAN1  TO    JYU-F14.       *>サカタ品単CD1
     MOVE PARA-HINTAN2  TO    JYU-F15.       *>サカタ品単CD2
     MOVE PARA-HINTAN3  TO    JYU-F16.       *>サカタ品単CD3
     MOVE PARA-TANABAN  TO    JYU-F17.       *>_番
     MOVE PARA-JYUTYUSU TO    JYU-F18.       *>発注数量
     MOVE PARA-SYUKASU  TO    JYU-F19.       *>出荷数量
     MOVE PARA-GENKA    TO    JYU-F20.       *>原価単価
     MOVE PARA-BAIKA    TO    JYU-F21.       *>売価単価
     MOVE PARA-GENKAKIN TO    JYU-F22.       *>原価金額
     MOVE PARA-BAIKAKIN TO    JYU-F23.       *>売価金額
     MOVE PARA-AITECD   TO    JYU-F24.       *>相手商品CD
     IF   PARA-HIKIATE  =  "1"
           MOVE SPACE   TO    JYU-F25        *>在庫引当区分
     ELSE
           MOVE "1"     TO    JYU-F25        *>在庫引当区分
     END-IF.
     MOVE PARA-DTKBN    TO    JYU-F26.       *>オンライン区分
     MOVE PARA-JDATE    TO    JYU-F01.       *>受信日
     MOVE PARA-JTIME    TO    JYU-F02.       *>受信時刻
     MOVE SYS-DATE      TO    JYU-F94.       *>登録日
     MOVE SYS-TIMEW     TO    JYU-F95.       *>登録時刻
     MOVE SYS-DATE      TO    JYU-F96.       *>更新日
     MOVE SYS-TIMEW     TO    JYU-F97.       *>更新時刻
     MOVE PARA-BUMON    TO    JYU-F98.       *>更新担当者部門
     MOVE PARA-TANCD    TO    JYU-F99.       *>更新担当者
*
     WRITE  JYU-REC.
*
 JYUZANF-WT-EXIT.
     EXIT.
****************************************************************
*              受注残ファイル更新
****************************************************************
 JYUZANF-REWT-SEC       SECTION.
*存在チェック
     MOVE PARA-SOKCD    TO    JYU-F09.       *>出荷場所
     MOVE PARA-HATYU    TO    JYU-F10.       *>発注日
     MOVE PARA-SYUKA    TO    JYU-F12.       *>出荷日
     MOVE PARA-SAKATACD TO    JYU-F13.       *>サカタ商品CD
     MOVE PARA-HINTAN1  TO    JYU-F14.       *>サカタ品単CD1
     MOVE PARA-HINTAN2  TO    JYU-F15.       *>サカタ品単CD2
     MOVE PARA-HINTAN3  TO    JYU-F16.       *>サカタ品単CD3
     MOVE PARA-TANABAN  TO    JYU-F17.       *>_番
     MOVE PARA-JYUTYUSU TO    JYU-F18.       *>発注数量
     MOVE PARA-SYUKASU  TO    JYU-F19.       *>出荷数量
     MOVE PARA-GENKA    TO    JYU-F20.       *>原価単価
     MOVE PARA-BAIKA    TO    JYU-F21.       *>売価単価
     MOVE PARA-GENKAKIN TO    JYU-F22.       *>原価金額
     MOVE PARA-BAIKAKIN TO    JYU-F23.       *>売価金額
     MOVE PARA-AITECD   TO    JYU-F24.       *>相手商品CD
     IF   PARA-HIKIATE  =  "1"
           MOVE SPACE   TO    JYU-F25        *>在庫引当区分
     ELSE
           MOVE "1"     TO    JYU-F25        *>在庫引当区分
     END-IF.
     MOVE PARA-DTKBN    TO    JYU-F26.       *>オンライン区分
     MOVE PARA-JDATE    TO    JYU-F01.       *>受信日
     MOVE PARA-JTIME    TO    JYU-F02.       *>受信時刻
     MOVE SYS-DATE      TO    JYU-F96.       *>更新日
     MOVE SYS-TIMEW     TO    JYU-F97.       *>更新時刻
     MOVE PARA-BUMON    TO    JYU-F98.       *>更新担当者部門
     MOVE PARA-TANCD    TO    JYU-F99.       *>更新担当者
*
     REWRITE  JYU-REC.
*
 JYUZANF-REWT-EXIT.
     EXIT.
****************************************************************
*              受注残ファイル削除
****************************************************************
 JYUZANF-DEL-SEC        SECTION.
*
     DELETE JYUZANF.
*
 JYUZANF-DEL-EXIT.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
*ファイルＣＬＯＳＥ
     CLOSE    JYUZANF.
*
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```

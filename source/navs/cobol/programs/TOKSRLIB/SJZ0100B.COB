****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    業務名　　　　　　　：　受信残管理システム　　　　　　　　
*    モジュール名　　　　：　受注残ファイル作成（オンライン）
*    作成日／更新日　　　：　2018/03/13
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　
*    処理概要　　　　　　：　パラメタにてバッチ番号を受け取り　
*                            売上伝票Ｆを読み、受注残Ｆを作成
*    　　　　　　　　　　　　する。
****************************************************************
*<履歴>*********************************************************
* XXXX/XX/XX XXXXXXXXX ＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮ
* 2018/03/13 高橋　　　新規作成（ＳＪＺ０１２０Ｂ流用）
*　　　　　　　　（対象バッチ番号の受注残Ｆを作成する。）
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SJZ0100B.
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
*---<<  売上伝票ファイル　　    >>---*
     SELECT   SHTDENF  ASSIGN     TO        DA-01-VI-SHTDENLA
                       ORGANIZATION         INDEXED
                       ACCESS     MODE      SEQUENTIAL
                       RECORD     KEY       DEN-F46  DEN-F47
                                            DEN-F01  DEN-F48
                                            DEN-F02  DEN-F04
                                            DEN-F051 DEN-F07
                                            DEN-F112 DEN-F03
                        FILE      STATUS    DEN-STATUS.
*---<<  商品コード変換テーブル  >>---*
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHO-F01
                                                 SHO-F02
                        FILE      STATUS    IS   SHO-STATUS.
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
*---<<  売上伝票ファイル　　    >>---*
 FD  SHTDENF.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*---<<  商品変換ＴＢＬ  >>---*
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
*---<<  受注残ファイル  >>---*
 FD  JYUZANF.
     COPY     JYUZANF   OF        XFDLIB
              JOINING   JYU       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*売上伝票ファイル退避
     COPY   SHTDENF  OF XFDLIB  JOINING   DE2  AS   PREFIX.
*
 01  SYS-DATE                PIC  9(08).
 01  STATUS-AREA.
     03  DEN-STATUS          PIC  X(02).
     03  SHO-STATUS          PIC  X(02).
     03  JYU-STATUS          PIC  X(02).
 01  WK-SURYO                PIC  9(10)V99.
 01  PSW-AREA.
     03  END-FLG             PIC  X(03)  VALUE SPACE.
     03  ZAMZAIF-INV-FLG     PIC  X(03)  VALUE SPACE.
     03  JYUZANF-INV-FLG     PIC  X(03)  VALUE SPACE.
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
       05  ERR-PG-ID         PIC  X(08)  VALUE "SJZ0100B".
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
 01  PARA-JDATE            PIC 9(08).
 01  PARA-JTIME            PIC 9(04).
 01  PARA-JTOKCD           PIC 9(08).
*--------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                     *
*--------------------------------------------------------------*
 PROCEDURE              DIVISION  USING  PARA-JDATE
                                         PARA-JTIME
                                         PARA-JTOKCD.
**
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE      SHTDENF.
     MOVE   "SHTDENLA"       TO   ERR-FL-ID.
     MOVE    DEN-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000          TO   PROGRAM-STATUS.
     STOP     RUN.
**
 FILEERR-SEC2        SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HSHOTBL.
     MOVE     "HSHOTBL"      TO   ERR-FL-ID.
     MOVE      SHO-STATUS    TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000          TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-SEC3        SECTION.
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
 SJZ0100B-START              SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     END-FLG   =    "END".
     PERFORM       END-SEC.
     STOP      RUN.
 SJZ0100B-END.
     EXIT.
****************************************************************
*      1.0 　　初期処理                                        *
****************************************************************
 INIT-SEC               SECTION.
*ファイルＯＰＥＮ
     OPEN     INPUT     HSHOTBL  SHTDENF.
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
*売上伝票ファイルスタート
     MOVE     SPACE           TO   DEN-REC.
     INITIALIZE                    DEN-REC.
     MOVE     PARA-JDATE      TO   DEN-F46.
     MOVE     PARA-JTIME      TO   DEN-F47.
     MOVE     PARA-JTOKCD     TO   DEN-F01.
     START  SHTDENF  KEY  IS  >  DEN-F46  DEN-F47  DEN-F01
                                 DEN-F48  DEN-F02  DEN-F04
                                 DEN-F051 DEN-F07  DEN-F112
                                 DEN-F03
         INVALID
         MOVE  "END"       TO   END-FLG
         DISPLAY NC"＃受注残Ｆ作成（ＯＮＬ）対象無１" UPON CONS
         MOVE   4000       TO   PROGRAM-STATUS
         GO                TO   INIT-END
     END-START.
*売上伝票ファイル読込
     PERFORM  SHTDENF-READ-SEC.
     IF  END-FLG  =  "END"
         DISPLAY NC"＃受注残Ｆ作成（ＯＮＬ）対象無２" UPON CONS
     END-IF.
*
 INIT-END.
     EXIT.
****************************************************************
*      ALL    売上伝票ファイル読込
****************************************************************
 SHTDENF-READ-SEC       SECTION.
*
     READ    SHTDENF
          AT END
             MOVE  "END"       TO   END-FLG
             GO                TO   SHTDENF-READ-EXIT
          NOT AT END
             ADD    1          TO   IN-CNT
     END-READ.
*件数表示
     IF    IN-CNT(5:3) =     "500"  OR  "000"
           DISPLAY  "READ-CNT = " IN-CNT   UPON CONS
     END-IF.
*バッチ番号チャック
     IF    PARA-JDATE  =  DEN-F46
     AND   PARA-JTIME  =  DEN-F47
     AND   PARA-JTOKCD =  DEN-F01
           CONTINUE
     ELSE
           MOVE  "END"       TO   END-FLG
           GO                TO   SHTDENF-READ-EXIT
     END-IF.
*商品変換ＴＢＬを索引する。
     MOVE     SPACE          TO   KEP-FLG.
     PERFORM  SHO-READ-SEC.
     IF  KEP-FLG  =  "1"
           ADD    1          TO   SKIP-CNT1
           GO                TO   SHTDENF-READ-SEC
     END-IF.
*
 SHTDENF-READ-EXIT.
     EXIT.
****************************************************************
*      2.0 　　メイン処理                                      *
****************************************************************
 MAIN-SEC               SECTION.
*
*---<  受注残ファイル作成  >---*
     PERFORM  JYUZANF-WT-SEC.
*---<  伝票データＦ　ＲＥＡＤ  >---*
     PERFORM  SHTDENF-READ-SEC.
*
 MAIN-END.
     EXIT.
****************************************************************
*      2.1     _番取得                                        *
****************************************************************
 SHO-READ-SEC           SECTION.
*
     MOVE    DEN-F01          TO   SHO-F01.
     MOVE    DEN-F25          TO   SHO-F02.
     READ    HSHOTBL
       INVALID      KEY
**********存在しない場合は、欠品ＦＬＧに”１”セット
          MOVE      "1"      TO   KEP-FLG
       NOT INVALID  KEY
**********存在した場合は、_番をＷＫに退避
          MOVE      SHO-F08  TO   WRK-TANA
     END-READ.
*
 SHO-READ-END.
     EXIT.
****************************************************************
*              受注残ファイル作成
****************************************************************
 JYUZANF-WT-SEC         SECTION.
*受注残ファイル初期化
     MOVE     SPACE     TO    JYU-REC.
     INITIALIZE               JYU-REC.
*存在チェック
     MOVE     DEN-F01   TO    JYU-F03.       *>取引先CD
     MOVE     DEN-F02   TO    JYU-F04.       *>伝票番号
     MOVE     DEN-F04   TO    JYU-F06.       *>相殺区分
     MOVE     DEN-F051  TO    JYU-F07.       *>伝票区分
     MOVE     DEN-F07   TO    JYU-F08.       *>店舗CD
     MOVE     DEN-F112  TO    JYU-F11.       *>納品日
     MOVE     DEN-F03   TO    JYU-F05.       *>行番号
     MOVE     DEN-F08   TO    JYU-F09.       *>出荷場所
     MOVE     DEN-F111  TO    JYU-F10.       *>発注日
     MOVE     DEN-F113  TO    JYU-F12.       *>出荷日
     MOVE     DEN-F1411 TO    JYU-F13.       *>サカタ商品CD
     MOVE     DEN-F1412(1:5) TO    JYU-F14.  *>サカタ品単CD1
     MOVE     DEN-F1412(6:2) TO    JYU-F15.  *>サカタ品単CD2
     MOVE     DEN-F1412(8:1) TO    JYU-F16.  *>サカタ品単CD3
     MOVE     WRK-TANA  TO    JYU-F17.       *>_番
     MOVE     DEN-F50   TO    JYU-F18.       *>発注数量
     MOVE     DEN-F15   TO    JYU-F19.       *>出荷数量
     MOVE     DEN-F172  TO    JYU-F20.       *>原価単価
     MOVE     DEN-F173  TO    JYU-F21.       *>売価単価
     COMPUTE  JYU-F22 = DEN-F172 * DEN-F15.  *>原価金額
     COMPUTE  JYU-F23 = DEN-F173 * DEN-F15.  *>売価金額
     MOVE     DEN-F25   TO    JYU-F24.       *>相手商品CD
     IF   DEN-F27D  =  "1"
           MOVE SPACE   TO    JYU-F25        *>在庫引当区分
     ELSE
           MOVE "1"     TO    JYU-F25        *>在庫引当区分
     END-IF.
     MOVE     "1"       TO    JYU-F26.       *>オンライン区分
     MOVE     DEN-F46   TO    JYU-F01.       *>受信日
     MOVE     DEN-F47   TO    JYU-F02.       *>受信時刻
     MOVE     SYS-DATE  TO    JYU-F94.       *>登録日
     MOVE     SYS-TIMEW TO    JYU-F95.       *>登録時刻
     MOVE     SYS-DATE  TO    JYU-F96.       *>更新日
     MOVE     SYS-TIMEW TO    JYU-F97.       *>更新時刻
     MOVE     "2920"    TO    JYU-F98.       *>更新担当者部門
     MOVE     "99"      TO    JYU-F99.       *>更新担当者
*受注残ファイル存在チェック
     READ  JYUZANF
           INVALID      MOVE "INV" TO JYUZANF-INV-FLG
           NOT  INVALID MOVE SPACE TO JYUZANF-INV-FLG
     END-READ.
*
     IF  JYUZANF-INV-FLG  =  "INV"
         WRITE  JYU-REC
         ADD    1        TO   JYU-CNT
     ELSE
         ADD    1        TO   SKIP-CNT2
     END-IF.
*
 JYUZANF-WT-EXIT.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
*
     DISPLAY "* SHTDENLA     (IN)= "  IN-CNT    " *" UPON CONS.
     DISPLAY "* HSHOTBL     (INV)= "  SKIP-CNT1 " *" UPON CONS.
     DISPLAY "* JYUZANF NOT (INV)= "  SKIP-CNT2 " *" UPON CONS.
     DISPLAY "* JYUZANF      (WT)= "  JYU-CNT   " *" UPON CONS.
*ファイルＣＬＯＳＥ
     CLOSE    SHTDENF    HSHOTBL  JYUZANF.
*
 END-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

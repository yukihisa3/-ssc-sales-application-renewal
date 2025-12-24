****************************************************************
*                                                              *
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　出荷検品システム                  *
*　　モジュール名　　　　：　検品明細Ｆ更新　　　　　　　　　　*
*　　作成日／更新日　　　：　08/02/18                          *
*　　作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*　　処理概要　　　　　　：　検品明細Ｆ・在庫マスタ　　　　　　*
*　　　　　　　　　　　　　　売上伝票Ｆ　を更新する　　　　　  *
*　　更新日／更新者　　　：　11/10/06 / YOSHIDA.M              *
*　　修正概要　　　　　　：　基幹サーバ統合                    *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SKE5120B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          08/02/18.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS
         YA        IS   CHR-2
         YB-21     IS   CHR-21
         YB        IS   CHR-15.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 検品明細Ｆ >>--*
     SELECT   RCVSYUF   ASSIGN         DA-01-S-RCVSYUF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         RCV-ST.
*----<< 累積検品明細Ｆ >>--*
     SELECT   RUISYUF   ASSIGN         DA-01-VI-RUISYUF
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  RUI-F04 RUI-F06 RUI-F07
                        STATUS         RUI-ST.
*---<<  商品在庫マスタ  >>---*
     SELECT   ZAMZAIF   ASSIGN    TO        DA-01-VI-ZAMZAIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   ZAI-F01
                                                 ZAI-F02
                                                 ZAI-F03
                        FILE      STATUS    IS   ZAI-ST.
*----<< 伝票データ >>--*
     SELECT   SHTDENF   ASSIGN    TO        DA-01-VI-SHTDENL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY  DEN-F01   DEN-F02
                                       DEN-F04   DEN-F051
***2011.10.06(DEN-F07,DEN-F112)
                                       DEN-F07   DEN-F112
                                       DEN-F03
                        FILE      STATUS    IS   DEN-ST.
*---<<  商品コード変換テーブル  >>---*
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHO-F01
                                                 SHO-F02
                        FILE      STATUS    IS   SHO-ST.
*プリント定義ファイル
     SELECT   PRTFILE   ASSIGN    TO        LP-04
                        FILE      STATUS    IS   PRT-ST.
*----<< 検品明細エラーＦ >>--*
     SELECT   RCVERRF   ASSIGN    TO        DA-01-S-RCVERRF
                        ORGANIZATION        SEQUENTIAL
                        STATUS              ERR-ST.
*---<<  商品名称マスタ  >>---*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F01
                        FILE      STATUS    IS   MEI-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 検品明細Ｆ >>--*
 FD  RCVSYUF            LABEL RECORD   IS   STANDARD
     BLOCK              CONTAINS       34   RECORDS.
     COPY        RCVDATSF    OF      XFDLIB
                 JOINING     RCV     PREFIX.
*----<< 累積検品明細Ｆ >>--*
 FD  RUISYUF            LABEL RECORD   IS   STANDARD
     BLOCK              CONTAINS       1    RECORDS.
     COPY        RCVDATF     OF      XFDLIB
                 JOINING     RUI     PREFIX.
*---<<  商品在庫マスタ  >>---*
 FD  ZAMZAIF.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZAI       PREFIX.
*----<< 伝票データ >>--*
 FD  SHTDENF            LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*---<<  商品変換テーブル  >>---*
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
*---<<  プリントファイル  >>---*
 FD  PRTFILE
     LABEL       RECORD    IS        OMITTED
     LINAGE                IS        66.
 01  PRT-REC.
     03  FILLER            PIC X(200).
*----<< 検品明細エラーＦ >>--*
 FD  RCVERRF            LABEL RECORD   IS   STANDARD
     BLOCK              CONTAINS       34   RECORDS.
     COPY        RCVDATSF    OF      XFDLIB
                 JOINING     ERR     PREFIX.
*---<<  商品名称マスタ  >>---*
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01).
     03  CHK-FLG        PIC  9(01).
     03  HMEIMS-INV-FLG PIC  X(03)  VALUE  SPACE.
 01  COUNTERS.
     03  IN-CNT         PIC  9(06).
     03  OUT-CNT        PIC  9(06).
*
*----<< ｴﾗｰﾒｯｾｰｼﾞ >>--*
 01  ERR-TBL.
   03    FILLER         PIC  N(14)     VALUE
                        NC"検品明細ファイル　重複".
   03    FILLER         PIC  N(14)     VALUE
                        NC"売上伝票ファイル未登録　　　".
   03    FILLER         PIC  N(14)     VALUE
                        NC"商品変換テーブル未登録　　　".
   03    FILLER         PIC  N(14)     VALUE
                        NC"在庫マスタ未登録　　　　　　".
   03    FILLER         PIC  N(14)     VALUE
                        NC"商品名称マスタ未登録　　　　".
   03    FILLER         PIC  N(14)     VALUE
                        NC"出荷数量＞発注数量　エラー　".
   03    FILLER         PIC  N(14)     VALUE
                        NC"出荷数量が０以下です。確認！".
 01  ERR-TBL-R          REDEFINES  ERR-TBL.
   03  ERR-MSG          PIC  N(14)     OCCURS   7.
*
*----<< ﾜｰｸ ｴﾘｱ >>--*
 01  WK-GOKEI           PIC  9(10)     VALUE  ZERO.
 01  WK-DEN-F27D        PIC  9(01)     VALUE  ZERO.
 01  ID                 PIC  9(01)     VALUE  ZERO.
 01  WK-DATE            PIC  9(08).
 01  WK-DATE-R          REDEFINES      WK-DATE.
     03  WK-DATE-R1     PIC  X(04).
     03  WK-DATE-R2     PIC  X(02).
     03  WK-DATE-R3     PIC  X(02).
 01  WK-RCV-F10         PIC  S9(9)V99  VALUE  ZERO.
 01  WK-RCV-F12         PIC  S9(9)V99  VALUE  ZERO.
 01  WK-SURYO           PIC  S9(9)V99  VALUE  ZERO.
 01  P-CNT              PIC  9(03)     VALUE  ZERO.
 01  L-CNT              PIC  9(02)     VALUE  99.
 01  HEN-DATE           PIC  9(08)     VALUE ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  RCV-ST             PIC  X(02).
 01  RUI-ST             PIC  X(02).
 01  ZAI-ST             PIC  X(02).
 01  DEN-ST             PIC  X(02).
 01  SHO-ST             PIC  X(02).
 01  PRT-ST             PIC  X(02).
 01  ERR-ST             PIC  X(02).
 01  MEI-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-YYMD           PIC  9(08).
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*
 01  HD1.
     03  FILLER                  PIC  X(40)  VALUE  SPACE.
     03  FILLER                  PIC  N(17)  VALUE
         NC"※※出荷検品取込エラーリスト※※"
                                 CHARACTER  TYPE  IS  CHR-21.
     03  FILLER                  PIC  X(20)  VALUE  SPACE.
     03  HD1-01                  PIC  9(04).
     03  FILLER                  PIC  N(01)  VALUE  NC"年"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  HD1-02                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"月"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  HD1-03                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"日"
                                 CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  HD1-04                  PIC  ZZ9.
     03  FILLER                  PIC  N(01)  VALUE  NC"頁"
                                 CHARACTER  TYPE  IS  CHR-2.
*
 01  HD2                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  FILLER                  PIC  N(02)  VALUE  NC"店舗".
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  FILLER                  PIC  N(03)  VALUE  NC"納品日".
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE NC"伝票番号".
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  FILLER                  PIC  N(01)  VALUE  NC"行".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  X(05)  VALUE  "JANCD".
     03  FILLER                  PIC  X(13)  VALUE  SPACE.
     03  FILLER                  PIC  N(03)  VALUE  NC"発注数".
     03  FILLER                  PIC  X(05)  VALUE  SPACE.
     03  FILLER                  PIC  N(03)  VALUE  NC"出荷数".
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  FILLER                  PIC  X(06)  VALUE  "ﾒｯｾｰｼﾞ".
*
 01  SEN                         CHARACTER  TYPE  IS  CHR-2.
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                  PIC  N(18)  VALUE
         NC"──────────────────".
 01  DT1                         CHARACTER  TYPE  IS  CHR-15.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-01                  PIC  9(05).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-02                  PIC  X(10).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-03                  PIC  9(09).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-04                  PIC  9(02).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-05                  PIC  X(13).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-06                  PIC  -,---,--9.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-07                  PIC  -,---,--9.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  DT1-08                  PIC  N(14).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 LINKAGE                SECTION.
 01  PARA-INCNT                  PIC  9(06).
 01  PARA-OUTCNT                 PIC  9(06).
 01  PARA-SOKCD                  PIC  X(02).
*
****************************************************************
 PROCEDURE              DIVISION USING       PARA-INCNT
                                             PARA-OUTCNT
                                             PARA-SOKCD.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 検品明細Ｆ >>--*
 RCV-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RCVSYUF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SKE5120B RCVSYUF    ERROR " RCV-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 累計出荷ファイル >>--*
 RUI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RUISYUF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SKE5120B RUISYUF    ERROR " RUI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 伝票データ >>--*
 SHTDENL1-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SKE5120B SHTDENL1   ERROR " DEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 商品コード変換テーブル >>--*
 HSHOTBL-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSHOTBL.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SKE5120B HSHOTBL    ERROR " SHO-ST    " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 在庫マスタ >>--*
 ZAMZAIF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZAMZAIF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SKE5120B ZAMZAIF    ERROR " ZAI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 検品明細エラーＦ >>--*
 RCVERRF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RCVERRF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SKE5120B RCVERRF ERROR " ERR-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 商品名称マスタ >>--*
 HMEIMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HMEIMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SKE5120B HMEIMS  ERROR " MEI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*　　　　メインモジュール　　　　　　　　　　　　　　　　　　　*
****************************************************************
 PROG-CNTL              SECTION.
     PERFORM  INIT-RTN.
     PERFORM  MAIN-RTN   UNTIL     END-FLG   =    1.
     PERFORM  END-RTN.
     STOP RUN.
 PROG-CNTL-EXIT.
     EXIT.
****************************************************************
*　　　　初期処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-RTN               SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKE5120B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
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
     MOVE     HEN-DATE(1:4)  TO   HD1-01.
     MOVE     HEN-DATE(5:2)  TO   HD1-02.
     MOVE     HEN-DATE(7:2)  TO   HD1-03.
     OPEN     INPUT     RCVSYUF HSHOTBL HMEIMS.
     OPEN     I-O       RUISYUF ZAMZAIF SHTDENF.
     OPEN     OUTPUT    PRTFILE RCVERRF.
*ワークエリア　クリア
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
*
     PERFORM  210-READ-SEC.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*
*累積データを参照
     MOVE     RCV-F04   TO   RUI-F04.
     MOVE     RCV-F06   TO   RUI-F06.
     MOVE     RCV-F07   TO   RUI-F07.
     PERFORM  220-READ-SEC.
*
     IF  CHK-FLG   =    0
         PERFORM   204-RUI-WRITE-SEC
         IF   CHK-FLG  NOT =  0
**************PERFORM   203-ERR-WRITE-SEC
              GO   TO   MAIN-01
         END-IF
     ELSE
*********重複エラー印字を解除する場合は下記をコメントアウト
*********PERFORM   203-ERR-WRITE-SEC
         GO   TO   MAIN-01
     END-IF.
*在庫マスタを参照
     PERFORM  201-ZAIKO-FIND-SEC.
*
     IF  CHK-FLG   =    0
*********DISPLAY "AAA"
         PERFORM   202-ZAIKO-UPD-SEC
     ELSE
*********DISPLAY "BBB"
         PERFORM   205-ZAIKO-WTD-SEC
         IF  HMEIMS-INV-FLG  =  "INV"
             MOVE      5     TO  CHK-FLG
             PERFORM   203-ERR-WRITE-SEC
         END-IF
     END-IF.
*
 MAIN-01.
*
     PERFORM  210-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     CLOSE    RCVSYUF   SHTDENF   HSHOTBL   ZAMZAIF
              RUISYUF   PRTFILE   HMEIMS.
*
     DISPLAY  "+++ INPUT      =" IN-CNT  " +++" UPON CONS.
     DISPLAY  "+++ OUTPUT     =" OUT-CNT " +++" UPON CONS.
     MOVE     IN-CNT         TO   PARA-INCNT.
     MOVE     OUT-CNT        TO   PARA-OUTCNT.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKE5120B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　在庫マスタ参照　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 201-ZAIKO-FIND-SEC     SECTION.
*    商品変換テーブル参照
*****MOVE     RCV-F01        TO   SHO-F01.
*****MOVE     RCV-F08        TO   SHO-F02.
*****PERFORM  240-READ-SEC.
*****IF CHK-FLG    NOT  =    0
*****   GO    TO   201-ZAIKO-FIND-EXIT
*****END-IF.
*    在庫マスタ参照
     MOVE     PARA-SOKCD     TO   ZAI-F01.
     MOVE     RCV-F13        TO   ZAI-F021.
     MOVE     RCV-F14        TO   ZAI-F022.
     MOVE     RCV-F15        TO   ZAI-F03.
*****DISPLAY "ZAI-F01  = " ZAI-F01  UPON CONS.
*****DISPLAY "ZAI-F021 = " ZAI-F021 UPON CONS.
*****DISPLAY "ZAI-F022 = " ZAI-F022 UPON CONS.
*****DISPLAY "ZAI-F03  = " ZAI-F03  UPON CONS.
     PERFORM  250-READ-SEC.
     IF CHK-FLG    NOT  =    0
        GO    TO   201-ZAIKO-FIND-EXIT
     END-IF.
*
 201-ZAIKO-FIND-EXIT.
     EXIT.
****************************************************************
*　　　　在庫マスタ更新　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 202-ZAIKO-UPD-SEC      SECTION.
*
*////検品明細の発注数量と出荷数量が同じ場合は抜ける
*****DISPLAY "WK-RCV-F12 = " WK-RCV-F12  UPON CONS.
*****DISPLAY "DEN-F15    = " DEN-F15     UPON CONS.
     IF  DEN-F15   =   WK-RCV-F12
*        売上伝票　フラグ更新
         MOVE 1        TO   DEN-F279
         GO            TO   202-ZAIKO-UPD-DEN-REWRITE
     ELSE
*        売上伝票　フラグ更新＆出荷数を数量に代入
         MOVE 1        TO   DEN-F279
         MOVE WK-RCV-F12 TO DEN-F15
         COMPUTE  DEN-F181  =  WK-RCV-F12  *  DEN-F172
         COMPUTE  DEN-F182  =  WK-RCV-F12  *  DEN-F173
     END-IF.
*////売上伝票の引当フラグが１の場合
*　　　　　　　　　　　　引当済数　－　発注数量
*****DISPLAY "DEN-F27D = " DEN-F27D UPON CONS.
     IF  DEN-F27D  =    1
         COMPUTE   ZAI-F28   =    ZAI-F28   -    WK-RCV-F10
*********DISPLAY "ZAI-F28 = " ZAI-F28 UPON CONS
     ELSE
         COMPUTE  ZAI-F27 =  ZAI-F27 -  WK-RCV-F10
     END-IF.
*
*////未出庫数　－　発注数
*////COMPUTE  ZAI-F27        =    ZAI-F27   -    WK-RCV-F10.
*
*  ＜引当判断＞　０以上の場合は引当ＯＫ
     COMPUTE  WK-SURYO       =    ZAI-F04   -    ZAI-F28
                                            -    WK-RCV-F12.
*****DISPLAY "WK-SURYO = " WK-SURYO UPON CONS.
     IF  WK-SURYO  >=   0
*        売上伝票の在庫引当フラグを１　出荷数 ADD 引当済数
         MOVE 1              TO   DEN-F27D
         COMPUTE   ZAI-F28   =    ZAI-F28   +    WK-RCV-F12
     ELSE
*        売上伝票の在庫引当フラグを０
         MOVE 0              TO   DEN-F27D
         COMPUTE  ZAI-F27  =  ZAI-F27  +  WK-RCV-F12
     END-IF.
*
*////出荷数 ADD 未出庫数
*****COMPUTE  ZAI-F27        =    ZAI-F27   +    WK-RCV-F12.
*
*  ＜在庫マスタ・売上伝票Ｆ更新＞
*
     REWRITE  ZAI-REC.
*
 202-ZAIKO-UPD-DEN-REWRITE.
*
*****DISPLAY "DEN-F15 = " DEN-F15 UPON CONS.
     REWRITE  DEN-REC.
*
 202-ZAIKO-UPD-EXIT.
     EXIT.
****************************************************************
*　　　　在庫マスタ作成　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 205-ZAIKO-WTD-SEC      SECTION.
*
*////検品明細の発注数量と出荷数量が同じ場合は抜ける
     IF  DEN-F15   =   WK-RCV-F12
         GO   TO   205-ZAIKO-WTD-010
     END-IF.
*商品在庫マスタ初期化
      MOVE      SPACE         TO   ZAI-REC.
      INITIALIZE                   ZAI-REC.
*商品在庫マスタ項目セット
      MOVE      PARA-SOKCD    TO   ZAI-F01.
      MOVE      RCV-F13       TO   ZAI-F021.
      MOVE      RCV-F14       TO   ZAI-F022.
      MOVE      RCV-F15       TO   ZAI-F03.
*未出庫数＝未出庫数＋数量
******DISPLAY "WK-RCV-F12 = " WK-RCV-F12 UPON CONS.
      COMPUTE   ZAI-F27       =    ZAI-F27  +  WK-RCV-F12
*商品名称マスタ読込み
      PERFORM   HMEIMS-READ-SEC.
*商品名称マスタ存在チェック
      IF  HMEIMS-INV-FLG  =  SPACE
          MOVE  MEI-F031      TO   ZAI-F30
          MOVE  HEN-DATE      TO   ZAI-F98
          MOVE  HEN-DATE      TO   ZAI-F99
          WRITE ZAI-REC
*         売上伝票の在庫引当フラグを０
          MOVE  0             TO   DEN-F27D
      ELSE
          GO                  TO   205-ZAIKO-WTD-EXIT
      END-IF.
 205-ZAIKO-WTD-010.
*     売上伝票　フラグ更新＆出荷数を数量に代入
      MOVE  1             TO   DEN-F279.
      MOVE  WK-RCV-F12    TO   DEN-F15.
      COMPUTE  DEN-F181  =  WK-RCV-F12  *  DEN-F172.
      COMPUTE  DEN-F182  =  WK-RCV-F12  *  DEN-F173.
      REWRITE  DEN-REC.
*
 205-ZAIKO-WTD-EXIT.
     EXIT.
****************************************************************
*　　　　エラーリスト印刷　　　　　　　　　　　　　　　　　　　*
****************************************************************
 203-ERR-WRITE-SEC      SECTION.
*
     IF  L-CNT > 60
         ADD       1         TO       P-CNT
         MOVE      P-CNT     TO       HD1-04
         IF   P-CNT     NOT  =    1
              WRITE     PRT-REC   FROM     HD1   AFTER     PAGE
         ELSE
              WRITE     PRT-REC   FROM     HD1
         END-IF
         WRITE     PRT-REC   FROM     SEN   AFTER     1
         WRITE     PRT-REC   FROM     HD2   AFTER     1
         WRITE     PRT-REC   FROM     SEN   AFTER     1
         MOVE      4         TO       L-CNT
     END-IF.
     MOVE     SPACE          TO        DT1.
     MOVE     RCV-F02        TO        DT1-01.
     MOVE     RCV-F05        TO        DT1-02.
     MOVE     RCV-F06        TO        DT1-03.
     MOVE     RCV-F07        TO        DT1-04.
     MOVE     RCV-F08        TO        DT1-05.
     MOVE  WK-RCV-F10        TO        DT1-06.
     MOVE  WK-RCV-F12        TO        DT1-07.
     IF   CHK-FLG  NOT =  0
          MOVE     ERR-MSG(CHK-FLG)
                             TO        DT1-08
     END-IF.
     WRITE         PRT-REC   FROM      DT1  AFTER     1.
*変換テーブルエラー時、エラーファイルへ出力
*## 2004/01/13 NAV UPDATE START ##
*****IF       CHK-FLG  =  3
     IF       CHK-FLG  =  3
     OR       CHK-FLG  =  6
     OR       CHK-FLG  =  7
*## 2004/01/13 NAV UPDATE END   ##
              MOVE  SPACE    TO        ERR-REC
              INITIALIZE               ERR-REC
              MOVE  RCV-REC  TO        ERR-REC
              WRITE ERR-REC
     END-IF.
     ADD           1         TO        L-CNT.
*
 203-ERR-WRITE-EXIT.
     EXIT.
****************************************************************
*　　　　累積検品明細Ｆ作成　　　　　　　　　　　　　　　　　　*
****************************************************************
 204-RUI-WRITE-SEC      SECTION.
*レコード初期化
     MOVE     SPACE          TO   RUI-REC.
     INITIALIZE                   RUI-REC.
*項目セット
     MOVE     RCV-F01        TO   RUI-F01.
     MOVE     RCV-F02        TO   RUI-F02.
     MOVE     RCV-F03(1:4)   TO   WK-DATE-R1.
     MOVE     RCV-F03(6:2)   TO   WK-DATE-R2.
     MOVE     RCV-F03(9:2)   TO   WK-DATE-R3.
     MOVE     WK-DATE        TO   RUI-F03.
     MOVE     RCV-F04        TO   RUI-F04.
     MOVE     ZERO           TO   RUI-F04.
     MOVE     RCV-F05(1:4)   TO   WK-DATE-R1.
     MOVE     RCV-F05(6:2)   TO   WK-DATE-R2.
     MOVE     RCV-F05(9:2)   TO   WK-DATE-R3.
     MOVE     WK-DATE        TO   RUI-F05.
     MOVE     RCV-F06        TO   RUI-F06.
     MOVE     RCV-F07        TO   RUI-F07.
     MOVE     RCV-F08        TO   RUI-F08.
     MOVE     WK-RCV-F10     TO   RUI-F09.
     MOVE     WK-RCV-F12     TO   RUI-F10.
     MOVE     RCV-F13        TO   RUI-F14.
     MOVE     RCV-F14        TO   RUI-F15.
     MOVE     RCV-F15        TO   RUI-F16.
*    商品変換テーブル参照
     MOVE     RCV-F01        TO   SHO-F01.
     MOVE     RCV-F08        TO   SHO-F02.
*****PERFORM  240-READ-SEC.
*****IF CHK-FLG  NOT =  0
*****   GO                   TO   CHK001
*****END-IF.
*    売上伝票ファイル参照
     MOVE     RCV-F01        TO   DEN-F01.
     MOVE     RCV-F06        TO   DEN-F02.
     MOVE     RCV-F07        TO   DEN-F03.
     MOVE     0              TO   DEN-F04.
     MOVE     40             TO   DEN-F051.
***2011.10.06 ST
     MOVE     RCV-F02        TO   DEN-F07.
     MOVE     RCV-F05        TO   DEN-F112.
***2011.10.06 EN
     PERFORM  230-READ-SEC.
*****DISPLAY "DEN-F50 = " DEN-F50 UPON CONS.
*****DISPLAY "RUI-F10 = " RUI-F10 UPON CONS.
*    発注数量と出荷数量の比較
     IF       CHK-FLG  =  0
              IF       DEN-F50  <  RUI-F10
                       MOVE      6    TO   CHK-FLG
                       PERFORM   203-ERR-WRITE-SEC
              END-IF
              IF       WK-RCV-F12  <  ZERO
                       MOVE      7    TO   CHK-FLG
                       PERFORM   203-ERR-WRITE-SEC
              END-IF
     ELSE
              PERFORM   203-ERR-WRITE-SEC
     END-IF.
*エラーチェック判定
 CHK001.
*****DISPLAY "CHK-FLG = " CHK-FLG UPON CONS.
     IF CHK-FLG  =  0
*       2003/11/07 発注数量は売上伝票のものをセットする。
        MOVE     DEN-F50     TO   RUI-F09
        WRITE    RUI-REC
        ADD      1               TO  OUT-CNT
     END-IF.
*
 204-RUI-WRITE-EXIT.
     EXIT.
****************************************************************
*　　　　インプットデータ読む　　　　　　　　　　　　　　　　　*
****************************************************************
 210-READ-SEC           SECTION.
*
     READ     RCVSYUF
        AT    END
              MOVE      1    TO   END-FLG
              GO   TO   210-READ-EXIT
        NOT AT END
              ADD       1    TO   IN-CNT
     END-READ.
*特別処理(2001/08/29)
     IF       RCV-F03  =  "2001/08/23"
              GO             TO   210-READ-SEC
     END-IF.
*
     IF  RCV-F09  =     ZERO
*        発注数量を１００分の１へ
         COMPUTE WK-RCV-F10 =  RCV-F10 / 100
     ELSE
*        発注数量を１００分の１へ
         COMPUTE  WK-RCV-F10 = ( RCV-F10 / 100 )   *   -1
     END-IF.
     IF  RCV-F11   =     ZERO
*        出荷数量を１００分の１へ
         COMPUTE WK-RCV-F12 =  RCV-F12 / 100
     ELSE
*        出荷数量を１００分の１へ
         COMPUTE  WK-RCV-F12 = ( RCV-F12 / 100 )   *   -1
     END-IF.
*
 210-READ-EXIT.
     EXIT.
****************************************************************
*　　　　アウトプットデータ読む　　　　　　　　　　　　　　　　*
****************************************************************
 220-READ-SEC           SECTION.
*
     READ     RUISYUF
        INVALID
              MOVE     0     TO   CHK-FLG
        NOT INVALID
              MOVE     1     TO   CHK-FLG
     END-READ.
*
 220-READ-EXIT.
     EXIT.
****************************************************************
*　　　　売上伝票ファイル読む　　　　　　　　　　　　　　　　　*
****************************************************************
 230-READ-SEC           SECTION.
*
     READ     SHTDENF
        INVALID
              MOVE     2     TO   CHK-FLG
        NOT INVALID
              MOVE     0     TO   CHK-FLG
     END-READ.
*
 230-READ-EXIT.
     EXIT.
****************************************************************
*　　　　商品変換テーブル読む　　　　　　　　　　　　　　　　　*
****************************************************************
 240-READ-SEC           SECTION.
*
     READ     HSHOTBL
        INVALID
              MOVE     3     TO   CHK-FLG
        NOT INVALID
              MOVE     0     TO   CHK-FLG
     END-READ.
*
 240-READ-EXIT.
     EXIT.
****************************************************************
*　　　　在庫マスタ読む　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 250-READ-SEC           SECTION.
*
     READ     ZAMZAIF
        INVALID
              MOVE     4     TO   CHK-FLG
        NOT INVALID
              MOVE     0     TO   CHK-FLG
     END-READ.
*
 250-READ-EXIT.
     EXIT.
****************************************************************
*                商品名称マスタ読込み                          *
****************************************************************
 HMEIMS-READ-SEC           SECTION.
*
     MOVE      RCV-F13     TO     MEI-F011.
     MOVE      RCV-F14     TO     MEI-F012.
     READ      HMEIMS
               INVALID
               MOVE      "INV"    TO    HMEIMS-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

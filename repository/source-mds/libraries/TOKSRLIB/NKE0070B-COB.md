# NKE0070B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NKE0070B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　出荷検品　　　　                  *
*　　モジュール名　　　　：　出荷検品確定データ更新　　　　　　*
*　　　　　　　　　　　　　　（オンライン手書共通）　　　　　　*
*　　作成日／作成者　　　：　2019/01/16  NAV                   *
*　　処理概要　　　　　　：　出荷検品システム側にで確定された　*
*　　　　　　　　　　　　　　出荷確定データより、売上伝票Ｆ・  *
*　　　　　　　　　　　　　　在庫マスタを更新、累積ファイルの  *
*　　　　　　　　　　　　　　作成を行う。　　　　　　　　　　  *
*　　更新日／更新者　　　：　                                  *
*　　処理概要　　　　　　：　　　　　　　　                    *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NKE0070B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/01/16.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS
         YA        IS   CHR-2
         YB-21     IS   CHR-21
         YB        IS   CHR-15.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 出荷確定Ｆ >>--*
     SELECT   RCVSYKXX  ASSIGN         DA-01-S-RCVSYKXX
                        ORGANIZATION   SEQUENTIAL
                        STATUS         RCV-ST.
*---<<  在庫マスタ  >>---*
     SELECT   ZAMZAIL1  ASSIGN    TO        DA-01-VI-ZAMZAIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   ZAI-F01
                                                 ZAI-F02
                                                 ZAI-F03
                        FILE      STATUS    IS   ZAI-ST.
*----<< 売上伝票ファイル >>--*
     SELECT   SHTDENL1  ASSIGN    TO        DA-01-VI-SHTDENL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY  DEN-F01   DEN-F02
                                       DEN-F04   DEN-F051
                                       DEN-F07   DEN-F112
                                       DEN-F03
                        FILE      STATUS    IS   DEN-ST.
*----<< 出荷確定累積Ｆ >>--*
     SELECT   RUISYKL1  ASSIGN         DA-01-VI-RUISYKL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  RUI-F01
                                       RUI-F06 RUI-F07 RUI-F09
                                       RUI-F10 RUI-F11
                        STATUS         RUI-ST.
*---<<  商品変換テーブル  >>---*
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHO-F01
                                                 SHO-F02
                        FILE      STATUS    IS   SHO-ST.
*---<<  商品名称マスタ  >>---*
     SELECT   MEIMS1    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F01
                        FILE      STATUS    IS   MEI-ST.
*プリント定義ファイル
*    SELECT   PRTFILE   ASSIGN    TO        LP-04
*                       FILE      STATUS    IS   PRT-ST.
*----<< 出荷確定エラーＦ >>--*
*    SELECT   RCVERRF   ASSIGN    TO        DA-01-S-RCVERRF
*                       ORGANIZATION        SEQUENTIAL
*                       STATUS              ERR-ST.
*----<< 送信済件数Ｆ >>--*
*    SELECT   SNDFINF   ASSIGN    TO        DA-01-S-SNDFINF
*                       ORGANIZATION        SEQUENTIAL
*                       STATUS              FIN-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 出荷確定Ｆ >>--*
 FD  RCVSYKXX            LABEL RECORD   IS   STANDARD
     BLOCK               CONTAINS       34   RECORDS.
     COPY        RCVSYKXX    OF      XFDLIB
                 JOINING     RCV     PREFIX.
*----<< 出荷確定累積Ｆ >>--*
 FD  RUISYKL1    LABEL RECORD   IS   STANDARD.
     COPY        RUISYKL1    OF      XFDLIB
                 JOINING     RUI     PREFIX.
*---<<  在庫マスタ  >>---*
 FD  ZAMZAIL1.
     COPY        ZAMZAIL1    OF      XFDLIB
                 JOINING     ZAI     PREFIX.
*----<< 売上伝票ファイル >>--*
 FD  SHTDENL1    LABEL       RECORD  IS   STANDARD.
     COPY        SHTDENL1    OF      XFDLIB
                 JOINING     DEN     PREFIX.
*---<<  商品変換テーブル  >>---*
 FD  SHOTBL1.
     COPY        SHOTBL1     OF      XFDLIB
                 JOINING     SHO     PREFIX.
*---<<  商品名称マスタ  >>---*
 FD  MEIMS1.
     COPY     MEIMS1    OF        XFDLIB
              JOINING   MEI       PREFIX.
*---<<  プリントファイル  >>---*
*FD  PRTFILE
*    LABEL       RECORD    IS        OMITTED
*    LINAGE                IS        66.
*01  PRT-REC.
*    03  FILLER            PIC X(200).
*----<< 出荷確定エラーＦ >>--*
*FD  RCVERRF            LABEL RECORD   IS   STANDARD
*    BLOCK              CONTAINS       34   RECORDS.
*    COPY        RCVDATSF    OF      XFDLIB
*                JOINING     ERR     PREFIX.
*----<< 送信済Ｆ >>--*
*FD  SNDFINF            LABEL RECORD   IS   STANDARD
*    BLOCK              CONTAINS       1    RECORDS.
*    COPY        SNDFINF     OF      XFDLIB
*                JOINING     FIN     PREFIX.
*
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01).
     03  CHK-FLG        PIC  9(01).
     03  MEI-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  TBL-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  DEN-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  ZAI-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  RUI-INV-FLG    PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG-1      PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG-2      PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG-3      PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG-4      PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG-5      PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG-6      PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG-7      PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG-8      PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG-9      PIC  X(03)  VALUE  SPACE.
     03  ERR-FLG-10     PIC  X(03)  VALUE  SPACE.
     03  DEN-UPD-FLG    PIC  X(03)  VALUE  SPACE.
 01  COUNTERS.
     03  IN-CNT         PIC  9(07).
     03  ERR-CNT        PIC  9(07).
     03  RUI-OUT-CNT    PIC  9(07).
     03  ZAI-OUT-CNT    PIC  9(07).
     03  ZAI-RWT-CNT    PIC  9(07).
     03  DEN-RWT-CNT    PIC  9(07).
*
*----<< ｴﾗｰﾒｯｾｰｼﾞ >>--*
 01  ERR-TBL.
   03    FILLER         PIC  N(14)     VALUE
                        NC"出荷確定データ　　重複".
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
 01  FIN-ST             PIC  X(02).
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
*
 01  WK-RCV-F03.
     03  WK-RCV-F031    PIC  9(04).
     03  WK-RCV-F032    PIC  9(02).
     03  WK-RCV-F033    PIC  9(02).
 01  WK-RCV-F05.
     03  WK-RCV-F051    PIC  9(04).
     03  WK-RCV-F052    PIC  9(02).
     03  WK-RCV-F053    PIC  9(02).
 01  WK-RCV-F05R REDEFINES WK-RCV-F05.
     03  WK-RCV-F05RR   PIC  9(08).
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*
*01  HD1.
*    03  FILLER                  PIC  X(40)  VALUE  SPACE.
*    03  FILLER                  PIC  N(17)  VALUE
*        NC"※※出荷検品取込エラーリスト※※"
*                                CHARACTER  TYPE  IS  CHR-21.
*    03  FILLER                  PIC  X(20)  VALUE  SPACE.
*    03  HD1-01                  PIC  9(04).
*    03  FILLER                  PIC  N(01)  VALUE  NC"年"
*                                CHARACTER  TYPE  IS  CHR-2.
*    03  HD1-02                  PIC  Z9.
*    03  FILLER                  PIC  N(01)  VALUE  NC"月"
*                                CHARACTER  TYPE  IS  CHR-2.
*    03  HD1-03                  PIC  Z9.
*    03  FILLER                  PIC  N(01)  VALUE  NC"日"
*                                CHARACTER  TYPE  IS  CHR-2.
*    03  FILLER                  PIC  X(03)  VALUE  SPACE.
*    03  HD1-04                  PIC  ZZ9.
*    03  FILLER                  PIC  N(01)  VALUE  NC"頁"
*                                CHARACTER  TYPE  IS  CHR-2.
*
*01  HD2                         CHARACTER  TYPE  IS  CHR-2.
*    03  FILLER                  PIC  X(02)  VALUE  SPACE.
*    03  FILLER                  PIC  N(02)  VALUE  NC"店舗".
*    03  FILLER                  PIC  X(03)  VALUE  SPACE.
*    03  FILLER                  PIC  N(03)  VALUE  NC"納品日".
*    03  FILLER                  PIC  X(03)  VALUE  SPACE.
*    03  FILLER                  PIC  N(04)  VALUE NC"伝票番号".
*    03  FILLER                  PIC  X(02)  VALUE  SPACE.
*    03  FILLER                  PIC  N(01)  VALUE  NC"行".
*    03  FILLER                  PIC  X(01)  VALUE  SPACE.
*    03  FILLER                  PIC  X(05)  VALUE  "JANCD".
*    03  FILLER                  PIC  X(13)  VALUE  SPACE.
*    03  FILLER                  PIC  N(03)  VALUE  NC"発注数".
*    03  FILLER                  PIC  X(05)  VALUE  SPACE.
*    03  FILLER                  PIC  N(03)  VALUE  NC"出荷数".
*    03  FILLER                  PIC  X(01)  VALUE  SPACE.
*    03  FILLER                  PIC  X(06)  VALUE  "ﾒｯｾｰｼﾞ".
*
*01  SEN                         CHARACTER  TYPE  IS  CHR-2.
*    03  FILLER                  PIC  N(25)  VALUE
*        NC"─────────────────────────".
*    03  FILLER                  PIC  N(25)  VALUE
*        NC"─────────────────────────".
*    03  FILLER                  PIC  N(18)  VALUE
*        NC"──────────────────".
*01  DT1                         CHARACTER  TYPE  IS  CHR-15.
*    03  FILLER                  PIC  X(01)  VALUE  SPACE.
*    03  DT1-01                  PIC  9(05).
*    03  FILLER                  PIC  X(01)  VALUE  SPACE.
*    03  DT1-02                  PIC  X(10).
*    03  FILLER                  PIC  X(01)  VALUE  SPACE.
*    03  DT1-03                  PIC  9(09).
*    03  FILLER                  PIC  X(01)  VALUE  SPACE.
*    03  DT1-04                  PIC  9(02).
*    03  FILLER                  PIC  X(01)  VALUE  SPACE.
*    03  DT1-05                  PIC  X(13).
*    03  FILLER                  PIC  X(01)  VALUE  SPACE.
*    03  DT1-06                  PIC  -,---,--9.
*    03  FILLER                  PIC  X(01)  VALUE  SPACE.
*    03  DT1-07                  PIC  -,---,--9.
*    03  FILLER                  PIC  X(01)  VALUE  SPACE.
*    03  DT1-08                  PIC  N(14).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
 LINKAGE                SECTION.
 01  PARA-IN-BUMCD               PIC  X(04).
 01  PARA-IN-TANCD               PIC  X(02).
 01  PARA-IN-SOKCD               PIC  X(02).
 01  PARA-IN-TDATE               PIC  9(08).
 01  PARA-IN-TTIME               PIC  9(06).
 01  PARA-OUT-KENSU              PIC  9(07).
*
****************************************************************
 PROCEDURE              DIVISION USING       PARA-IN-BUMCD
                                             PARA-IN-TANCD
                                             PARA-IN-SOKCD
                                             PARA-IN-TDATE
                                             PARA-IN-TTIME
                                             PARA-OUT-KENSU.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 出荷確定Ｆ >>--*
 RCV-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RCVSYKXX.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### NKE0070B RCVSYKXX    ERROR " RCV-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 出荷確定累積ファイル >>--*
 RUI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RUISYKL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### NKE0070B RUISYKL1    ERROR " RUI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 売上伝票ファイル >>--*
 SHTDENL1-ERR           SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### NKE0070B SHTDENL1   ERROR " DEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 商品変換テーブル >>--*
 SHOTBL1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHOTBL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### NKE0070B SHOTBL1    ERROR " SHO-ST    " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 在庫マスタ >>--*
 ZAMZAIL1-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZAMZAIL1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### NKE0070B ZAMZAIL1    ERROR " ZAI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 商品名称マスタ >>--*
 MEIMS1-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      MEIMS1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### NKE0070B MEIMS1  ERROR " MEI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 出荷確定エラーＦ >>--*
*RCVERRF-ERR            SECTION.
*    USE AFTER     EXCEPTION PROCEDURE      RCVERRF.
*    ACCEPT   SYS-DATE       FROM DATE.
*    ACCEPT   SYS-TIME       FROM TIME.
*    MOVE     "4000"         TO   PROGRAM-STATUS.
*    DISPLAY  "### NKE0070B RCVERRF ERROR " ERR-ST " "
*             SYS-YY "." SYS-MM "." SYS-DD " "
*             SYS-HH ":" SYS-MN ":" SYS-SS " ###"
*                                      UPON CONS.
*    STOP     RUN.
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
*T↓
*    DISPLAY "INIT-RTN" UPON CONS.
*T↑
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** NKE0070B START *** "
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
*    MOVE     HEN-DATE(1:4)  TO   HD1-01.
*    MOVE     HEN-DATE(5:2)  TO   HD1-02.
*    MOVE     HEN-DATE(7:2)  TO   HD1-03.
     OPEN     INPUT     RCVSYKXX SHOTBL1  MEIMS1.
     OPEN     I-O       RUISYKL1 ZAMZAIL1 SHTDENL1.
*    OPEN     OUTPUT    PRTFILE  RCVERRF.
*    OPEN     EXTEND    SNDFINF.
*ワークエリア　クリア
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
*
     PERFORM  RCV-READ-SEC.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*T↓
*    DISPLAY "MAIN-RTN" UPON CONS.
*T↑
*
*累積データを参照
     MOVE     PARA-IN-SOKCD  TO   RUI-F01.
     MOVE     RCV-F01        TO   RUI-F06.
     MOVE     RCV-F02        TO   RUI-F07.
     MOVE     WK-RCV-F05RR   TO   RUI-F09.
     MOVE     RCV-F06        TO   RUI-F10.
     MOVE     RCV-F07        TO   RUI-F11.
     PERFORM  RUI-FIND-SEC.
*T↓
*    DISPLAY "RUI-INV-FLG = " RUI-INV-FLG UPON CONS.
*T↑
*
     IF  RUI-INV-FLG  = "INV"
         CONTINUE
*    IF  CHK-FLG   =    0
*        PERFORM   RUI-WRITE-SEC
*        IF   CHK-FLG  NOT =  0
*             GO   TO   MAIN-01
*        END-IF
     ELSE
         GO   TO   MAIN-01
     END-IF.
*売上伝票データを参照
     INITIALIZE                   DEN-REC.
     MOVE     RCV-F01        TO   DEN-F01.
     MOVE     RCV-F06        TO   DEN-F02.
     MOVE     0              TO   DEN-F04.
     MOVE     40             TO   DEN-F051.
     MOVE     RCV-F02        TO   DEN-F07.
     MOVE     WK-RCV-F05RR   TO   DEN-F112.
     MOVE     RCV-F07        TO   DEN-F03.
*T↓
*    DISPLAY "DEN-F01     =" DEN-F01     UPON CONS.
*    DISPLAY "DEN-F02     =" DEN-F02     UPON CONS.
*    DISPLAY "DEN-F04     =" DEN-F04     UPON CONS.
*    DISPLAY "DEN-F051    =" DEN-F051    UPON CONS.
*    DISPLAY "DEN-F07     =" DEN-F07     UPON CONS.
*    DISPLAY "DEN-F112    =" DEN-F112    UPON CONS.
*    DISPLAY "DEN-F03     =" DEN-F03     UPON CONS.
*T↑
     PERFORM  DEN-FIND-SEC.
*T↓
*    DISPLAY "DEN-INV-FLG =" DEN-INV-FLG UPON CONS.
*    DISPLAY "RCV-F01     =" RCV-F01     UPON CONS.
*    DISPLAY "RCV-F06     =" RCV-F06     UPON CONS.
*    DISPLAY "RCV-F02     =" RCV-F02     UPON CONS.
*    DISPLAY "WK-RCV-F05  =" WK-RCV-F05  UPON CONS.
*    DISPLAY "RCV-F07     =" RCV-F07     UPON CONS.
*    DISPLAY "DEN-F50     =" DEN-F50     UPON CONS.
*    DISPLAY "WK-RCV-F10  =" WK-RCV-F10  UPON CONS.
*    DISPLAY "WK-RCV-F12  =" WK-RCV-F12  UPON CONS.
*T↑
*    発注数量と出荷数量の比較
     IF       DEN-INV-FLG  = "HIT"
*----IF       CHK-FLG  =  0
              IF       DEN-F50  <  WK-RCV-F12
                       MOVE   "ERR"   TO   ERR-FLG-5
                       MOVE      6    TO   CHK-FLG
              END-IF
     END-IF.
*
*商品変換ＴＢＬを参照
     MOVE     RCV-F01        TO   SHO-F01.
     MOVE     RCV-F08        TO   SHO-F02.
     PERFORM  TBL-FIND-SEC.
*
*商品名称マスタを参照
     MOVE     RCV-F13        TO   MEI-F011.
     MOVE     RCV-F14        TO   MEI-F012.
     PERFORM  MEI-FIND-SEC.
*
*更新続行判定
     IF     ( DEN-INV-FLG  = "HIT" ) AND
            ( ERR-FLG-5    = "   " ) AND
            ( ERR-FLG-6    = "   " )
              CONTINUE
     ELSE
              GO   TO   MAIN-01
     END-IF.
*
*売上伝票データを更新
*在庫マスタを参照・更新
     INITIALIZE                   ZAI-REC.
     MOVE     PARA-IN-SOKCD  TO   ZAI-F01.
     MOVE     RCV-F13        TO   ZAI-F021.
     MOVE     RCV-F14        TO   ZAI-F022.
     MOVE     RCV-F15        TO   ZAI-F03.
     PERFORM  ZAI-FIND-SEC.
*T↓
*    DISPLAY "ZAI-INV-FLG = " ZAI-INV-FLG UPON CONS.
*T↑
*
     MOVE     "   "    TO   DEN-UPD-FLG.
     IF  ZAI-INV-FLG   =  "HIT"
*    IF  CHK-FLG   =    0
*        MOVE     "UPD"    TO   DEN-UPD-FLG
         PERFORM   ZAI-UPD-SEC
     ELSE
*        MOVE     "UPD"    TO   DEN-UPD-FLG
         PERFORM   ZAI-WT-SEC
     END-IF.
*
 MAIN-01.
*
     PERFORM  RUI-WRITE-SEC.
*
 MAIN-02.
*
     INITIALIZE      FLAGS.
     MOVE     "   "  TO   DEN-UPD-FLG.
     PERFORM  RCV-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*T↓
*    DISPLAY "END-RTN" UPON CONS.
*T↑
*
     CLOSE    RCVSYKXX   SHTDENL1   SHOTBL1   ZAMZAIL1
              RUISYKL1   MEIMS1.
*
     DISPLAY  "+++ INPUT      =" IN-CNT  " +++" UPON CONS.
     DISPLAY  "+++ OUTPUT     =" RUI-OUT-CNT " +++" UPON CONS.
     DISPLAY  "+++ ERROR DATA =" ERR-CNT     " +++" UPON CONS.
     DISPLAY  "+++ ZAI_OUTPUT =" ZAI-OUT-CNT " +++" UPON CONS.
     DISPLAY  "+++ ZAI_UPDATE =" ZAI-RWT-CNT " +++" UPON CONS.
     DISPLAY  "+++ DEN_UPDATE =" DEN-RWT-CNT " +++" UPON CONS.
*    MOVE     IN-CNT         TO   PARA-INCNT.
     MOVE     RUI-OUT-CNT    TO   PARA-OUT-KENSU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** NKE0070B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　インプットデータ読　　　　　　　　　　　　　　　　　　*
****************************************************************
 RCV-READ-SEC           SECTION.
*T↓
*    DISPLAY "------------"  UPON CONS.
*    DISPLAY "RCV-READ-SEC"  UPON CONS.
*T↑
*
     READ     RCVSYKXX
        AT    END
              MOVE      1    TO   END-FLG
              GO   TO   RCV-READ-EXIT
        NOT AT END
              ADD       1    TO   IN-CNT
*T↓
*             DISPLAY "IN-CNT=" IN-CNT UPON CONS
*T↑
     END-READ.
*特別処理(2001/08/29)
*    IF       RCV-F03  =  "2001/08/23"
*             GO             TO   RCV-READ-SEC
*    END-IF.
*
     IF  RCV-F09  =     "0"
*        発注数量を１００分の１
         COMPUTE  WK-RCV-F10 =  RCV-F10 / 100
     ELSE
*        発注数量を１００分の１＆負数値化
         COMPUTE  WK-RCV-F10 = ( RCV-F10 / 100 )   *   -1
     END-IF.
     IF  RCV-F11   =     "0"
*        出荷数量を１００分の１　
         COMPUTE  WK-RCV-F12 =  RCV-F12 / 100
     ELSE
*        出荷数量を１００分の１＆負数値化
         COMPUTE  WK-RCV-F12 = ( RCV-F12 / 100 )   *   -1
     END-IF.
*
*    日付を数値型に
*    発送完了日
     MOVE  RCV-F03(1:4)     TO   WK-RCV-F031.
     MOVE  RCV-F03(6:2)     TO   WK-RCV-F032.
     MOVE  RCV-F03(9:2)     TO   WK-RCV-F033.
*T↓
*    DISPLAY "WK-RCV-F03 = " WK-RCV-F03 UPON CONS.
*T↑
*
*    納品日
     MOVE  RCV-F05(1:4)     TO   WK-RCV-F051.
     MOVE  RCV-F05(6:2)     TO   WK-RCV-F052.
     MOVE  RCV-F05(9:2)     TO   WK-RCV-F053.
*T↓
*    DISPLAY "WK-RCV-F05 = " WK-RCV-F05 UPON CONS.
*T↑
*
*    出荷数がマイナスはエラー
     IF    WK-RCV-F12  <  ZERO
           MOVE   "ERR"     TO   ERR-FLG-6
           MOVE      7      TO   CHK-FLG
     END-IF.
*
 RCV-READ-EXIT.
     EXIT.
****************************************************************
*　　　　出荷確定累積ファイル存在チェック　　　　　　　　　　　*
****************************************************************
 RUI-FIND-SEC           SECTION.
*T↓
*    DISPLAY "RUI-FIND-SEC" UPON CONS.
*T↑
*
     READ     RUISYKL1
        INVALID
              MOVE    "INV"  TO   RUI-INV-FLG
              MOVE    "   "  TO   ERR-FLG-1
              MOVE     0     TO   CHK-FLG
        NOT INVALID
              MOVE    "HIT"  TO   RUI-INV-FLG
              MOVE    "ERR"  TO   ERR-FLG-1
              MOVE     1     TO   CHK-FLG
     END-READ.
*
 RUI-FIND-EXIT.
     EXIT.
****************************************************************
*　　　　在庫マスタ存在チェック　　　　　　　　　　　　　　　　*
****************************************************************
 ZAI-FIND-SEC     SECTION.
*T↓
*    DISPLAY "ZAI-FIND-SEC" UPON CONS.
*T↑
*    在庫マスタ参照
*****DISPLAY "ZAI-F01  = " ZAI-F01  UPON CONS.
*****DISPLAY "ZAI-F021 = " ZAI-F021 UPON CONS.
*****DISPLAY "ZAI-F022 = " ZAI-F022 UPON CONS.
*****DISPLAY "ZAI-F03  = " ZAI-F03  UPON CONS.
     READ     ZAMZAIL1
        INVALID
              MOVE   "INV"   TO   ZAI-INV-FLG
              MOVE     4     TO   CHK-FLG
        NOT INVALID
              MOVE   "HIT"   TO   ZAI-INV-FLG
              MOVE     0     TO   CHK-FLG
     END-READ.
*
 ZAI-FIND-EXIT.
     EXIT.
****************************************************************
*　　　　在庫マスタ更新　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 ZAI-UPD-SEC      SECTION.
*T↓
*    DISPLAY "ZAI-UPD-SEC" UPON CONS.
*T↑
*
*////売上伝票_発注数量 = 出荷確定_出荷数量 の場合は
*    在庫マスタは更新しない。
*T↓
*****DISPLAY "WK-RCV-F12 = " WK-RCV-F12  UPON CONS.
*    DISPLAY "DEN-F15    = " DEN-F15     UPON CONS.
*T↑
     IF  DEN-F15   =   WK-RCV-F12
*        売上伝票　フラグ更新
         MOVE 1        TO   DEN-F279
         GO            TO   ZAI-UPD-DEN-REWRITE
     ELSE
*        売上伝票　フラグ更新＆出荷数を数量に代入
         MOVE 1        TO   DEN-F279
         MOVE WK-RCV-F12 TO DEN-F15
         COMPUTE  DEN-F181  =  WK-RCV-F12  *  DEN-F172
         COMPUTE  DEN-F182  =  WK-RCV-F12  *  DEN-F173
*T↓
*        DISPLAY "DEN-F181=RCV-F12*DEN-F172.." DEN-F181 UPON CONS
*        DISPLAY "DEN-F182=RCV-F12*DEN-F173.." DEN-F182 UPON CONS
*T↑
     END-IF.
*////売上伝票の引当フラグが１の場合
*　　　　　　　　　　　　引当済数　－　発注数量
*T↓
*    DISPLAY "DEN-F27D = " DEN-F27D UPON CONS.
*T↑
     IF  DEN-F27D  =    1
         COMPUTE  ZAI-F27 =   ZAI-F27 -  WK-RCV-F10
         COMPUTE  ZAI-F28 =   ZAI-F28 -  WK-RCV-F10
*T↓
*        DISPLAY "ZAI-F27=ZAI-F27-WK-RCV-F10.." ZAI-F27 UPON CONS
*        DISPLAY "ZAI-F28=ZAI-F28-WK-RCV-F10.." ZAI-F28 UPON CONS
*T↑
     ELSE
         COMPUTE  ZAI-F27 =   ZAI-F27 -  WK-RCV-F10
*T↓
*        DISPLAY "ZAI-F27 = ZAI-F27 - WK-RCV-F10.."
*                                               ZAI-F27 UPON CONS
*T↑
     END-IF.
*
*  ＜引当判断＞　０以上の場合は引当ＯＫ
     COMPUTE  WK-SURYO       =    ZAI-F04   -    ZAI-F28
                                            -    WK-RCV-F12.
*T↓
*    DISPLAY "WK-SURYO = ZAI-F04 - ZAI-F28 - RCV-F12.."
*                                            WK-SURYO UPON CONS.
*T↑
     IF  WK-SURYO  >=   0
*        売上伝票の在庫引当フラグを１　出荷数 ADD 引当済数
         MOVE 1              TO   DEN-F27D
         MOVE HEN-DATE       TO   ZAI-F99
         COMPUTE   ZAI-F27   =    ZAI-F27   +    WK-RCV-F12
         COMPUTE   ZAI-F28   =    ZAI-F28   +    WK-RCV-F12
*T↓
*        DISPLAY "ZAI-F27 = ZAI-F27 - WK-RCV-F12.."
*                                               ZAI-F27 UPON CONS
*        DISPLAY "ZAI-F28 = ZAI-F28 - WK-RCV-F12.."
*                                               ZAI-F28 UPON CONS
*T↑
     ELSE
*        売上伝票の在庫引当フラグを０
         MOVE 0              TO   DEN-F27D
         MOVE HEN-DATE       TO   ZAI-F99
         COMPUTE  ZAI-F27  =  ZAI-F27  +  WK-RCV-F12
*T↓
*        DISPLAY "ZAI-F27 = ZAI-F27 - WK-RCV-F10.."
*                                               ZAI-F27 UPON CONS
*T↑
     END-IF.
*
*  ＜在庫マスタ・売上伝票Ｆ更新＞
*
     REWRITE  ZAI-REC.
     ADD      1       TO     ZAI-RWT-CNT.
*T↓
*    DISPLAY "ZAI-RWT-CNT =" ZAI-RWT-CNT UPON CONS.
*T↑
*
 ZAI-UPD-DEN-REWRITE.
*
*****DISPLAY "DEN-F15 = " DEN-F15 UPON CONS.
     REWRITE  DEN-REC.
     MOVE    "UPD"    TO     DEN-UPD-FLG.
     ADD      1       TO     DEN-RWT-CNT.
*T↓
*    DISPLAY "DEN-RWT-CNT =" DEN-RWT-CNT UPON CONS.
*T↑
*
 ZAI-UPD-EXIT.
     EXIT.
****************************************************************
*　　　　在庫マスタ作成　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 ZAI-WT-SEC      SECTION.
*T↓
*    DISPLAY "ZAI-WT-SEC"  UPON CONS.
*T↑
*
*////売上伝票_発注数量 = 出荷確定_出荷数量 の場合は
*    在庫マスタは作成しない。
*T↓
*    DISPLAY "DEN-F15 =" DEN-F15 UPON CONS.
*T↑
     IF  DEN-F15   =   WK-RCV-F12
         GO   TO   ZAI-WT-010
     END-IF.
*商品在庫マスタ初期化
      MOVE      SPACE         TO   ZAI-REC.
      INITIALIZE                   ZAI-REC.
*商品在庫マスタ項目セット
      MOVE      PARA-IN-SOKCD TO   ZAI-F01.
      MOVE      RCV-F13       TO   ZAI-F021.
      MOVE      RCV-F14       TO   ZAI-F022.
      MOVE      RCV-F15       TO   ZAI-F03.
*未出庫数＝未出庫数＋数量
*T↓
*     DISPLAY "ZAI-F27 = " ZAI-F27 UPON CONS.
*T↑
      COMPUTE   ZAI-F27       =    ZAI-F27  +  WK-RCV-F12.
*T↓
*     DISPLAY "ZAI-F27 = ZAI-F27 + -WK-RCV-F12.."
*                                     ZAI-F27    UPON CONS.
*T↑
*引当済数＝引当済数＋数量
*T↓
*     DISPLAY "ZAI-F28 = " ZAI-F28 UPON CONS.
*T↑
      COMPUTE   ZAI-F28       =    ZAI-F28  +  WK-RCV-F12
*T↓
*     DISPLAY "ZAI-F28 = ZAI-F28 + -WK-RCV-F12.."
*                                     ZAI-F28    UPON CONS.
*T↑
*商品名称マスタ読込み
*     PERFORM   MEI-FIND-SEC.
*T↓
*     DISPLAY  "MEI-INV-FLG =" MEI-INV-FLG UPON CONS.
*T↑
*商品名称マスタ存在チェック
      IF    MEI-INV-FLG  =  "HIT"
            MOVE  MEI-F031   TO   ZAI-F30
      END-IF.
      MOVE  HEN-DATE         TO   ZAI-F98.
      MOVE  HEN-DATE         TO   ZAI-F99.
      WRITE ZAI-REC.
      ADD   1                TO   ZAI-OUT-CNT.
*T↓
*     DISPLAY  "ZAI-OUT-CNT =" ZAI-OUT-CNT UPON CONS
*T↑
*     売上伝票の在庫引当フラグを０
      MOVE  0             TO   DEN-F27D.
*
*---- IF  MEI-INV-FLG  =  "HIT"
*         MOVE  MEI-F031      TO   ZAI-F30
*         MOVE  HEN-DATE      TO   ZAI-F98
*         MOVE  HEN-DATE      TO   ZAI-F99
*         WRITE ZAI-REC
*         ADD   1             TO   ZAI-OUT-CNT
*         売上伝票の在庫引当フラグを０
*         MOVE  0             TO   DEN-F27D
*     ELSE
*         GO                  TO   ZAI-WT-EXIT
*---- END-IF.
*
 ZAI-WT-010.
*     売上伝票　フラグ更新＆出荷数を数量に代入
      MOVE  1             TO   DEN-F279.
      MOVE  WK-RCV-F12    TO   DEN-F15.
*T↓
*     DISPLAY "DEN-F181 = " DEN-F181 UPON CONS.
*T↑
      COMPUTE  DEN-F181  =  WK-RCV-F12  *  DEN-F172.
*T↓
*     DISPLAY "DEN-F181 = WK-RCV-F12 * DEN-F172.."
*                                      DEN-F181   UPON CONS.
*T↑
*T↓
*     DISPLAY "DEN-F182 = " DEN-F182 UPON CONS.
*T↑
      COMPUTE  DEN-F182  =  WK-RCV-F12  *  DEN-F173.
*T↓
*     DISPLAY "DEN-F182 = WK-RCV-F12 * DEN-F173.."
*                                      DEN-F182   UPON CONS.
*T↑
      REWRITE  DEN-REC.
      MOVE    "UPD"              TO    DEN-UPD-FLG.
      ADD      1                 TO    DEN-RWT-CNT.
*T↓
*     DISPLAY  "DEN-RWT-CNT =" DEN-RWT-CNT UPON CONS.
*T↑
*
 ZAI-WT-EXIT.
     EXIT.
****************************************************************
*    　　出荷確定累積Ｆ作成　　　　　　　　　　　　　　　　　　*
****************************************************************
 RUI-WRITE-SEC      SECTION.
*T↓
*     DISPLAY  "RUI-WRITE-SEC"  UPON CONS.
*T↑
*レコード初期化
     MOVE     SPACE          TO   RUI-REC.
     INITIALIZE                   RUI-REC.
*項目セット
*エラー内容０１
     IF       ERR-FLG-1  =  "ERR"
              MOVE      "1"  TO   RUI-F89
                                  RUI-F90
     ELSE
              MOVE      " "  TO   RUI-F90
     END-IF.
*エラー内容０２
     IF       ERR-FLG-2  =  "ERR"
              MOVE      "1"  TO   RUI-F89
                                  RUI-F91
     ELSE
              MOVE      " "  TO   RUI-F91
     END-IF.
*エラー内容０３
     IF       ERR-FLG-3  =  "ERR"
              MOVE      "1"  TO   RUI-F89
                                  RUI-F92
     ELSE
              MOVE      " "  TO   RUI-F92
     END-IF.
*エラー内容０４
     IF       ERR-FLG-4  =  "ERR"
              MOVE      "1"  TO   RUI-F89
                                  RUI-F93
     ELSE
              MOVE      " "  TO   RUI-F93
     END-IF.
*エラー内容０５
     IF       ERR-FLG-5  =  "ERR"
              MOVE      "1"  TO   RUI-F89
                                  RUI-F94
     ELSE
              MOVE      " "  TO   RUI-F94
     END-IF.
*エラー内容０６
     IF       ERR-FLG-6  =  "ERR"
              MOVE      "1"  TO   RUI-F89
                                  RUI-F95
     ELSE
              MOVE      " "  TO   RUI-F95
     END-IF.
*エラー内容０７
     IF       ERR-FLG-7  =  "ERR"
              MOVE      "1"  TO   RUI-F89
                                  RUI-F96
     ELSE
              MOVE      " "  TO   RUI-F96
     END-IF.
*エラー内容０８
     IF       ERR-FLG-8  =  "ERR"
              MOVE      "1"  TO   RUI-F89
                                  RUI-F97
     ELSE
              MOVE      " "  TO   RUI-F97
     END-IF.
*エラー内容０９
     IF       ERR-FLG-9  =  "ERR"
              MOVE      "1"  TO   RUI-F89
                                  RUI-F98
     ELSE
              MOVE      " "  TO   RUI-F98
     END-IF.
*エラー内容１０
     IF       ERR-FLG-10 =  "ERR"
              MOVE      "1"  TO   RUI-F89
                                  RUI-F99
     ELSE
              MOVE      " "  TO   RUI-F99
     END-IF.
*エラー件数カウント
     IF       RUI-F89    =  "1"
              ADD        1   TO   ERR-CNT
*T↓
*             DISPLAY "ERR-CNT = " ERR-CNT UPON CONS
*T↑
     END-IF.
*T↓
*             DISPLAY "ERR-FLG-1 = " ERR-FLG-1 UPON CONS
*             DISPLAY "ERR-FLG-2 = " ERR-FLG-2 UPON CONS
*             DISPLAY "ERR-FLG-3 = " ERR-FLG-3 UPON CONS
*             DISPLAY "ERR-FLG-4 = " ERR-FLG-4 UPON CONS
*             DISPLAY "ERR-FLG-5 = " ERR-FLG-5 UPON CONS
*             DISPLAY "ERR-FLG-6 = " ERR-FLG-6 UPON CONS
*T↑
*倉庫ＣＤ
     MOVE     PARA-IN-SOKCD  TO   RUI-F01.
*取込日付
     MOVE     PARA-IN-TDATE  TO   RUI-F02.
*取込時刻
     MOVE     PARA-IN-TTIME  TO   RUI-F03.
*    MOVE     RCV-F03(1:4)   TO   WK-DATE-R1.
*    MOVE     RCV-F03(6:2)   TO   WK-DATE-R2.
*    MOVE     RCV-F03(9:2)   TO   WK-DATE-R3.
*    MOVE     WK-DATE        TO   RUI-F03.
*部門ＣＤ
     MOVE     PARA-IN-BUMCD  TO   RUI-F04.
*担当者ＣＤ
     MOVE     PARA-IN-TANCD  TO   RUI-F05.
*取引先ＣＤ
     MOVE     RCV-F01        TO   RUI-F06.
*店舗ＣＤ
     MOVE     RCV-F02        TO   RUI-F07.
*発送完了日
     MOVE     WK-RCV-F03     TO   RUI-F08.
*納品日
     MOVE     WK-RCV-F05     TO   RUI-F09.
*伝票番号
     MOVE     RCV-F06        TO   RUI-F10.
*行番号
     MOVE     RCV-F07        TO   RUI-F11.
*ＪＡＮＣＤ
     MOVE     RCV-F08        TO   RUI-F12.
*発注数量
     IF       RUI-F89   = " "
              MOVE  DEN-F50       TO   RUI-F13
     ELSE
              MOVE  WK-RCV-F10    TO   RUI-F13
     END-IF.
*出荷数量
     MOVE     WK-RCV-F12     TO   RUI-F14.
*棚番
     MOVE     RCV-F15        TO   RUI-F15.
*サカタ商品ＣＤ
     MOVE     RCV-F13        TO   RUI-F16.
*サカタ品単ＣＤ
     MOVE     RCV-F14        TO   RUI-F17.
*相手商品ＣＤ
     IF       RUI-F89   = " "
              MOVE     DEN-F25    TO   RUI-F18
     ELSE
              MOVE     RCV-F08    TO   RUI-F18
     END-IF.
*更新区分
     IF       DEN-UPD-FLG  = "UPD"
              MOVE  "1"      TO   RUI-F88
     ELSE
              MOVE  " "      TO   RUI-F88
     END-IF.
*
*----商品変換テーブル参照
*----MOVE     RCV-F01        TO   SHO-F01.
*----MOVE     RCV-F08        TO   SHO-F02.
*****PERFORM  TBL-FIND-SEC.
*****IF CHK-FLG  NOT =  0
*****   GO                   TO   CHK001
*****END-IF.
*----売上伝票ファイル参照
*----MOVE     RCV-F01        TO   DEN-F01.
*----MOVE     RCV-F06        TO   DEN-F02.
*----MOVE     RCV-F07        TO   DEN-F03.
*----MOVE     0              TO   DEN-F04.
*----MOVE     40             TO   DEN-F051.
*----MOVE     RCV-F02        TO   DEN-F07.
*----MOVE     RCV-F05        TO   DEN-F112.
*----PERFORM  DEN-FIND-SEC.
*****DISPLAY "DEN-F50 = " DEN-F50 UPON CONS.
*****DISPLAY "RUI-F10 = " RUI-F10 UPON CONS.
*----発注数量と出荷数量の比較
*----IF       CHK-FLG  =  0
*----         IF       DEN-F50  <  RUI-F10
*----                  MOVE   "ERR"   TO   ERR-FLG-5
*----                  MOVE      6    TO   CHK-FLG
***********************PERFORM   203-ERR-WRITE-SEC
*----         END-IF
*----         IF       WK-RCV-F12  <  ZERO
*----                  MOVE   "ERR"   TO   ERR-FLG-6
*----                  MOVE      7    TO   CHK-FLG
***********************PERFORM   203-ERR-WRITE-SEC
*----         END-IF
*----ELSE
*----         PERFORM   203-ERR-WRITE-SEC
*----END-IF.
*エラーチェック判定
*CHK001.
*****DISPLAY "CHK-FLG = " CHK-FLG UPON CONS.
*    IF CHK-FLG  =  0
*       2003/11/07 発注数量は売上伝票のものをセットする。
*       MOVE     DEN-F50     TO   RUI-F09
*       WRITE    RUI-REC
*       ADD      1               TO  RUI-OUT-CNT
*データセット（検品システム側データ削除）
*       MOVE     SPACE          TO   FIN-REC
*       INITIALIZE                   FIN-REC
*
*       MOVE     RCV-F01        TO   FIN-F01
*       MOVE     RCV-F06        TO   FIN-F02
*       MOVE     RCV-F07        TO   FIN-F03
*       MOVE     X"0D0A"        TO   FIN-F04
*
*       WRITE    FIN-REC
*    END-IF.
*
     WRITE    RUI-REC.
     ADD      1               TO  RUI-OUT-CNT.
*T↓
*    DISPLAY "RUI-OUT-CNT = " RUI-OUT-CNT UPON CONS.
*T↑
*
 RUI-WRITE-EXIT.
     EXIT.
****************************************************************
*　　　　売上伝票ファイル読　　　　　　　　　　　　　　　　　　*
****************************************************************
 DEN-FIND-SEC           SECTION.
*T↓
*    DISPLAY "DEN-FIND-SEC"  UPON CONS.
*T↑
*
     READ     SHTDENL1
        INVALID
              MOVE   "INV"   TO   DEN-INV-FLG
              MOVE   "ERR"   TO   ERR-FLG-2
              MOVE     2     TO   CHK-FLG
        NOT INVALID
              MOVE   "HIT"   TO   DEN-INV-FLG
              MOVE   "   "   TO   ERR-FLG-2
              MOVE     0     TO   CHK-FLG
     END-READ.
*
 DEN-FIND-EXIT.
     EXIT.
****************************************************************
*　　　　商品変換テーブル読　　　　　　　　　　　　　　　　　　*
****************************************************************
 TBL-FIND-SEC           SECTION.
*T↓
*    DISPLAY "TBL-FIND-SEC"  UPON CONS.
*T↑
*
     READ     SHOTBL1
        INVALID
              MOVE   "INV"   TO   TBL-INV-FLG
              MOVE   "ERR"   TO   ERR-FLG-3
              MOVE     3     TO   CHK-FLG
        NOT INVALID
              MOVE   "HIT"   TO   TBL-INV-FLG
              MOVE   "   "   TO   ERR-FLG-3
              MOVE     0     TO   CHK-FLG
     END-READ.
*
 TBL-FIND-EXIT.
     EXIT.
****************************************************************
*                商品名称マスタ読　　                          *
****************************************************************
 MEI-FIND-SEC           SECTION.
*T↓
*    DISPLAY "MEI-FIND-SEC"  UPON CONS.
*T↑
*
     READ      MEIMS1
        INVALID
               MOVE      "INV"    TO    MEI-INV-FLG
               MOVE      "ERR"    TO    ERR-FLG-4
        NOT  INVALID
               MOVE      "HIT"    TO    MEI-INV-FLG
               MOVE      "   "    TO    ERR-FLG-4
     END-READ.
*
 MEI-FIND-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```

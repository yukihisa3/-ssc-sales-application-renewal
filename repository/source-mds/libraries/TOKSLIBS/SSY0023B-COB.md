# SSY0023B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY0023B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　：　サカタのタネ（株）　　　　　　　　　　*
*    業務名　　　　　：　出荷管理サブシステム　　　　　　　　　*
*    モジュール名　　：　伝票発行場所振分（手書）　　　　　　　*
*    作成日　　　　　：　1999/10/13                            *
*    作成者　　　　　：　NAV TAKAHASHI                         *
*    更新日　　　　　：　2008/08/11                            *
*    更新者　　　　　：　NAV TAKAHASHI                         *
*    更新内容　　　　：  抽出条件追加　　　　　　　　　　　　*
*                        ※承認日が０の場合は抽出対象としない。*
*                                                              *
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 SSY0023B.
 ENVIRONMENT                 DIVISION.
 CONFIGURATION               SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     CONSOLE       IS        CONS
     YA            IS        YA
     YB-21         IS        YB-21.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*伝票データＦ
     SELECT      SHTDENF     ASSIGN    TO        DA-01-VI-SHTDENL6
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      SEQUENTIAL
                             RECORD    KEY       DEN-F277 DEN-F274
                                                 DEN-F09  DEN-F02
                                                 DEN-F03
                             FILE      STATUS    DEN-ST.
*条件ファイル
     SELECT      HJYOKEN     ASSIGN    TO        DA-01-VI-JYOKEN1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       JYO-F01
                                                 JYO-F02
                             FILE      STATUS    JYO-ST.
*店舗マスタ
     SELECT      HTENMS      ASSIGN    TO        DA-01-VI-TENMS1
                             ORGANIZATION        INDEXED
                             ACCESS    MODE      RANDOM
                             RECORD    KEY       TEN-F52
                                                 TEN-F011
                             FILE      STATUS    TEN-ST.
*手書きＦＪＮＬ１～２０
     SELECT      JHTDEN01   ASSIGN    TO        DA-01-S-JHTDEN01
                             FILE      STATUS    D01-ST.
*
     SELECT      JHTDEN02   ASSIGN    TO        DA-01-S-JHTDEN02
                             FILE      STATUS    D02-ST.
*
     SELECT      JHTDEN03   ASSIGN    TO        DA-01-S-JHTDEN03
                             FILE      STATUS    D03-ST.
*
     SELECT      JHTDEN04   ASSIGN    TO        DA-01-S-JHTDEN04
                             FILE      STATUS    D04-ST.
*
     SELECT      JHTDEN05   ASSIGN    TO        DA-01-S-JHTDEN05
                             FILE      STATUS    D05-ST.
*
     SELECT      JHTDEN06   ASSIGN    TO        DA-01-S-JHTDEN06
                             FILE      STATUS    D06-ST.
*
     SELECT      JHTDEN07   ASSIGN    TO        DA-01-S-JHTDEN07
                             FILE      STATUS    D07-ST.
*
     SELECT      JHTDEN08   ASSIGN    TO        DA-01-S-JHTDEN08
                             FILE      STATUS    D08-ST.
*
     SELECT      JHTDEN09   ASSIGN    TO        DA-01-S-JHTDEN09
                             FILE      STATUS    D09-ST.
*
     SELECT      JHTDEN10   ASSIGN    TO        DA-01-S-JHTDEN10
                             FILE      STATUS    D10-ST.
*
     SELECT      JHTDEN11   ASSIGN    TO        DA-01-S-JHTDEN11
                             FILE      STATUS    D11-ST.
*
     SELECT      JHTDEN12   ASSIGN    TO        DA-01-S-JHTDEN12
                             FILE      STATUS    D12-ST.
*
     SELECT      JHTDEN13   ASSIGN    TO        DA-01-S-JHTDEN13
                             FILE      STATUS    D13-ST.
*
     SELECT      JHTDEN14   ASSIGN    TO        DA-01-S-JHTDEN14
                             FILE      STATUS    D14-ST.
*
     SELECT      JHTDEN15   ASSIGN    TO        DA-01-S-JHTDEN15
                             FILE      STATUS    D15-ST.
*
     SELECT      JHTDEN16   ASSIGN    TO        DA-01-S-JHTDEN16
                             FILE      STATUS    D16-ST.
*
     SELECT      JHTDEN17   ASSIGN    TO        DA-01-S-JHTDEN17
                             FILE      STATUS    D17-ST.
*
     SELECT      JHTDEN18   ASSIGN    TO        DA-01-S-JHTDEN18
                             FILE      STATUS    D18-ST.
*
     SELECT      JHTDEN19   ASSIGN    TO        DA-01-S-JHTDEN19
                             FILE      STATUS    D19-ST.
*
     SELECT      JHTDEN20   ASSIGN    TO        DA-01-S-JHTDEN20
                             FILE      STATUS    D20-ST.
*****<<  プリント Ｆ  >>**************************************
     SELECT      PRINTF      ASSIGN    TO        LP-04-PRTF.
*商品コード変換テーブル（取引先ＣＤ＋出荷場所＋自社＋品単）
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL2
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHO-F01
                                                 SHO-F04
                                                 SHO-F031
                                                 SHO-F032
                        FILE      STATUS    IS   SHO-ST.
*商品コード変換テーブル（取引先ＣＤ＋出荷場所＋量販店商品）
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SH1-F01
                                                 SH1-F02
                        FILE      STATUS    IS   SH1-ST.
*
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*伝票データＦ
 FD  SHTDENF
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    16        RECORDS.
     COPY        SHTDENF     OF        XFDLIB
     JOINING     DEN         AS        PREFIX.
*条件ファイル
 FD  HJYOKEN
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS     8        RECORDS.
     COPY        HJYOKEN     OF        XFDLIB
     JOINING     JYO         AS        PREFIX.
*店舗マスタ
 FD  HTENMS
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS     8        RECORDS.
     COPY        HTENMS      OF        XFDLIB
     JOINING     TEN         AS        PREFIX.
*手書きＦＪＮＬ１～２０
*　　本社用
 FD  JHTDEN01
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
 01  D01-REC.
     03  D01-REC1           PIC  X(230).
     03  D01-REC15          PIC  9(001).
     03  D01-REC2           PIC  9(003)  PACKED-DECIMAL.
     03  D01-REC3           PIC  X(787).
*
*　　共栄用
 FD  JHTDEN02
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
 01  D02-REC.
     03  D02-REC1           PIC  X(230).
     03  D02-REC15          PIC  9(001).
     03  D02-REC2           PIC  9(003)  PACKED-DECIMAL.
     03  D02-REC3           PIC  X(787).
*
*　　フバサミ
 FD  JHTDEN03
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
 01  D03-REC.
     03  D03-REC1           PIC  X(230).
     03  D03-REC15          PIC  9(001).
     03  D03-REC2           PIC  9(003)  PACKED-DECIMAL.
     03  D03-REC3           PIC  X(787).
*
*　　鴻巣
 FD  JHTDEN04
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
 01  D04-REC.
     03  D04-REC1           PIC  X(230).
     03  D04-REC15          PIC  9(001).
     03  D04-REC2           PIC  9(003)  PACKED-DECIMAL.
     03  D04-REC3           PIC  X(787).
*
*　　西尾
 FD  JHTDEN05
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
 01  D05-REC.
     03  D05-REC1           PIC  X(230).
     03  D05-REC15          PIC  9(001).
     03  D05-REC2           PIC  9(003)  PACKED-DECIMAL.
     03  D05-REC3           PIC  X(787).
*
*　　大和
 FD  JHTDEN06
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
 01  D06-REC.
     03  D06-REC1           PIC  X(230).
     03  D06-REC15          PIC  9(001).
     03  D06-REC2           PIC  9(003)  PACKED-DECIMAL.
     03  D06-REC3           PIC  X(787).
*
*　　手綱
 FD  JHTDEN07
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
 01  D07-REC.
     03  D07-REC1           PIC  X(230).
     03  D07-REC15          PIC  9(001).
     03  D07-REC2           PIC  9(003)  PACKED-DECIMAL.
     03  D07-REC3           PIC  X(787).
*
*　　？？？
 FD  JHTDEN08
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
 01  D08-REC.
     03  D08-REC1           PIC  X(230).
     03  D08-REC15          PIC  9(001).
     03  D08-REC2           PIC  9(003)  PACKED-DECIMAL.
     03  D08-REC3           PIC  X(787).
*
*　　？？？
 FD  JHTDEN09
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
 01  D09-REC.
     03  D09-REC1           PIC  X(230).
     03  D09-REC15          PIC  9(001).
     03  D09-REC2           PIC  9(003)  PACKED-DECIMAL.
     03  D09-REC3           PIC  X(787).
*
*　　？？？
 FD  JHTDEN10
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
 01  D10-REC.
     03  D10-REC1           PIC  X(230).
     03  D10-REC15          PIC  9(001).
     03  D10-REC2           PIC  9(003)  PACKED-DECIMAL.
     03  D10-REC3           PIC  X(787).
*　　？？？
 FD  JHTDEN11
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
 01  D11-REC.
     03  D11-REC1           PIC  X(230).
     03  D11-REC15          PIC  9(001).
     03  D11-REC2           PIC  9(003)  PACKED-DECIMAL.
     03  D11-REC3           PIC  X(787).
*
*　　？？？
 FD  JHTDEN12
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
 01  D12-REC.
     03  D12-REC1           PIC  X(230).
     03  D12-REC15          PIC  9(001).
     03  D12-REC2           PIC  9(003)  PACKED-DECIMAL.
     03  D12-REC3           PIC  X(787).
*
*　　？？？
 FD  JHTDEN13
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
 01  D13-REC.
     03  D13-REC1           PIC  X(230).
     03  D13-REC15          PIC  9(001).
     03  D13-REC2           PIC  9(003)  PACKED-DECIMAL.
     03  D13-REC3           PIC  X(787).
*
*　　？？？
 FD  JHTDEN14
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
 01  D14-REC.
     03  D14-REC1           PIC  X(230).
     03  D14-REC15          PIC  9(001).
     03  D14-REC2           PIC  9(003)  PACKED-DECIMAL.
     03  D14-REC3           PIC  X(787).
*
*　　？？？
 FD  JHTDEN15
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
 01  D15-REC.
     03  D15-REC1           PIC  X(230).
     03  D15-REC15          PIC  9(001).
     03  D15-REC2           PIC  9(003)  PACKED-DECIMAL.
     03  D15-REC3           PIC  X(787).
*
*　　？？？
 FD  JHTDEN16
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
 01  D16-REC.
     03  D16-REC1           PIC  X(230).
     03  D16-REC15          PIC  9(001).
     03  D16-REC2           PIC  9(003)  PACKED-DECIMAL.
     03  D16-REC3           PIC  X(787).
*
*　　？？？
 FD  JHTDEN17
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
 01  D17-REC.
     03  D17-REC1           PIC  X(230).
     03  D17-REC15          PIC  9(001).
     03  D17-REC2           PIC  9(003)  PACKED-DECIMAL.
     03  D17-REC3           PIC  X(787).
*
*　　？？？
 FD  JHTDEN18
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
 01  D18-REC.
     03  D18-REC1           PIC  X(230).
     03  D18-REC15          PIC  9(001).
     03  D18-REC2           PIC  9(003)  PACKED-DECIMAL.
     03  D18-REC3           PIC  X(787).
*
*　　？？？
 FD  JHTDEN19
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
 01  D19-REC.
     03  D19-REC1           PIC  X(230).
     03  D19-REC15          PIC  9(001).
     03  D19-REC2           PIC  9(003)  PACKED-DECIMAL.
     03  D19-REC3           PIC  X(787).
*
*　　？？？
 FD  JHTDEN20
     LABEL       RECORD      IS        STANDARD
     BLOCK       CONTAINS    1         RECORDS.
 01  D20-REC.
     03  D20-REC1           PIC  X(230).
     03  D20-REC15          PIC  9(001).
     03  D20-REC2           PIC  9(003)  PACKED-DECIMAL.
     03  D20-REC3           PIC  X(787).
*
****************************************************************
*    FILE = プリント　ファイル                                 *
****************************************************************
 FD  PRINTF.
 01  PRINT-REC                    PIC       X(200).
*
******************************************************************
*    商品変換テーブル_
******************************************************************
 FD  HSHOTBL.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SHO       PREFIX.
******************************************************************
*    商品変換テーブル_
******************************************************************
 FD  SHOTBL1.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   SH1       PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  IN-DATA             PIC  X(01)  VALUE  SPACE.
     03  DEN-ST              PIC  X(02)  VALUE  SPACE.
     03  JYO-ST              PIC  X(02)  VALUE  SPACE.
     03  TEN-ST              PIC  X(02)  VALUE  SPACE.
     03  D01-ST              PIC  X(02)  VALUE  SPACE.
     03  D02-ST              PIC  X(02)  VALUE  SPACE.
     03  D03-ST              PIC  X(02)  VALUE  SPACE.
     03  D04-ST              PIC  X(02)  VALUE  SPACE.
     03  D05-ST              PIC  X(02)  VALUE  SPACE.
     03  D06-ST              PIC  X(02)  VALUE  SPACE.
     03  D07-ST              PIC  X(02)  VALUE  SPACE.
     03  D08-ST              PIC  X(02)  VALUE  SPACE.
     03  D09-ST              PIC  X(02)  VALUE  SPACE.
     03  D10-ST              PIC  X(02)  VALUE  SPACE.
     03  D11-ST              PIC  X(02)  VALUE  SPACE.
     03  D12-ST              PIC  X(02)  VALUE  SPACE.
     03  D13-ST              PIC  X(02)  VALUE  SPACE.
     03  D14-ST              PIC  X(02)  VALUE  SPACE.
     03  D15-ST              PIC  X(02)  VALUE  SPACE.
     03  D16-ST              PIC  X(02)  VALUE  SPACE.
     03  D17-ST              PIC  X(02)  VALUE  SPACE.
     03  D18-ST              PIC  X(02)  VALUE  SPACE.
     03  D19-ST              PIC  X(02)  VALUE  SPACE.
     03  D20-ST              PIC  X(02)  VALUE  SPACE.
     03  SHO-ST              PIC  X(02)  VALUE  SPACE.
     03  SH1-ST              PIC  X(02)  VALUE  SPACE.
 01  WK-AREA.
     03  END-FLG             PIC  9(01)  VALUE  ZERO.
     03  SKIP-FLG            PIC  9(01)  VALUE  ZERO.
     03  INV-SW              PIC  9(01)  VALUE  ZERO.
     03  I                   PIC  9(01)  VALUE  ZERO.
     03  IX                  PIC  9(02)  VALUE  ZERO.
     03  WK-BASHO            PIC  9(02)  VALUE  ZERO.
     03  BR-BASHO            PIC  9(02)  VALUE  ZERO.
     03  CHK-FLG             PIC  X(03)  VALUE  SPACE.
     03  CHK1-FLG            PIC  X(03)  VALUE  SPACE.
     03  TEN-INV-FLG         PIC  X(03)  VALUE  SPACE.
     03  WK-URIBA            PIC  9(03)  PACKED-DECIMAL.
     03  WK-DENPYO           PIC  9(09)  VALUE  ZERO.
     03  WK-DENNO            PIC  9(09)  VALUE  ZERO.
     03  SYS-DATE.
         05  SYS-YY          PIC  9(02)  VALUE  ZERO.
         05  SYS-MM          PIC  9(02)  VALUE  ZERO.
         05  SYS-DD          PIC  9(02)  VALUE  ZERO.
     03  WK-FLCD.
         05  WK-FLCD1        PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD2        PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD3        PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD4        PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD5        PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD6        PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD7        PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD8        PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD9        PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD10       PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD11       PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD12       PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD13       PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD14       PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD15       PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD16       PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD17       PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD18       PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD19       PIC  9(05)  VALUE  ZERO.
         05  WK-FLCD20       PIC  9(05)  VALUE  ZERO.
     03  WK-FLCDR  REDEFINES WK-FLCD.
         05  WK-FLCDT        PIC  9(05)  OCCURS 20.
     03  WK-RTCD.
         05  WK-RTCD1        PIC  9(05)  VALUE  ZERO.
         05  WK-RTCD2        PIC  9(05)  VALUE  ZERO.
         05  WK-RTCD3        PIC  9(05)  VALUE  ZERO.
         05  WK-RTCD4        PIC  9(05)  VALUE  ZERO.
         05  WK-RTCD5        PIC  9(05)  VALUE  ZERO.
         05  WK-RTCD6        PIC  9(05)  VALUE  ZERO.
         05  WK-RTCD7        PIC  9(05)  VALUE  ZERO.
         05  WK-RTCD8        PIC  9(05)  VALUE  ZERO.
         05  WK-RTCD9        PIC  9(05)  VALUE  ZERO.
         05  WK-RTCD10       PIC  9(05)  VALUE  ZERO.
         05  WK-RTCD11       PIC  9(05)  VALUE  ZERO.
         05  WK-RTCD12       PIC  9(05)  VALUE  ZERO.
         05  WK-RTCD13       PIC  9(05)  VALUE  ZERO.
         05  WK-RTCD14       PIC  9(05)  VALUE  ZERO.
         05  WK-RTCD15       PIC  9(05)  VALUE  ZERO.
         05  WK-RTCD16       PIC  9(05)  VALUE  ZERO.
         05  WK-RTCD17       PIC  9(05)  VALUE  ZERO.
         05  WK-RTCD18       PIC  9(05)  VALUE  ZERO.
         05  WK-RTCD19       PIC  9(05)  VALUE  ZERO.
         05  WK-RTCD20       PIC  9(05)  VALUE  ZERO.
     03  WK-RTCDR  REDEFINES WK-RTCD.
         05  WK-RTCDT        PIC  9(05)  OCCURS 20.
     03  WK-RTNM.
         05  WK-RTNM1        PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM2        PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM3        PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM4        PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM5        PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM6        PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM7        PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM8        PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM9        PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM10       PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM11       PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM12       PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM13       PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM14       PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM15       PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM16       PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM17       PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM18       PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM19       PIC  N(10)  VALUE  SPACE.
         05  WK-RTNM20       PIC  N(10)  VALUE  SPACE.
     03  WK-RTNMR  REDEFINES WK-RTNM.
         05  WK-RTNMT        PIC  N(10)  OCCURS 20.
     03  WK-RTCNT.
         05  WK-RTCNT1       PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT2       PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT3       PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT4       PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT5       PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT6       PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT7       PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT8       PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT9       PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT10      PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT11      PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT12      PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT13      PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT14      PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT15      PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT16      PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT17      PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT18      PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT19      PIC  9(05)  VALUE  ZERO.
         05  WK-RTCNT20      PIC  9(05)  VALUE  ZERO.
     03  WK-RTCNTR REDEFINES WK-RTCNT.
         05  WK-RTCNTT       PIC  9(05)  OCCURS 20.
     03  WK-DENCNT.
         05  WK-DENCNT1       PIC  9(05)  VALUE  ZERO.
         05  WK-DENCNT2       PIC  9(05)  VALUE  ZERO.
         05  WK-DENCNT3       PIC  9(05)  VALUE  ZERO.
         05  WK-DENCNT4       PIC  9(05)  VALUE  ZERO.
         05  WK-DENCNT5       PIC  9(05)  VALUE  ZERO.
         05  WK-DENCNT6       PIC  9(05)  VALUE  ZERO.
         05  WK-DENCNT7       PIC  9(05)  VALUE  ZERO.
         05  WK-DENCNT8       PIC  9(05)  VALUE  ZERO.
         05  WK-DENCNT9       PIC  9(05)  VALUE  ZERO.
         05  WK-DENCNT10      PIC  9(05)  VALUE  ZERO.
         05  WK-DENCNT11      PIC  9(05)  VALUE  ZERO.
         05  WK-DENCNT12      PIC  9(05)  VALUE  ZERO.
         05  WK-DENCNT13      PIC  9(05)  VALUE  ZERO.
         05  WK-DENCNT14      PIC  9(05)  VALUE  ZERO.
         05  WK-DENCNT15      PIC  9(05)  VALUE  ZERO.
         05  WK-DENCNT16      PIC  9(05)  VALUE  ZERO.
         05  WK-DENCNT17      PIC  9(05)  VALUE  ZERO.
         05  WK-DENCNT18      PIC  9(05)  VALUE  ZERO.
         05  WK-DENCNT19      PIC  9(05)  VALUE  ZERO.
         05  WK-DENCNT20      PIC  9(05)  VALUE  ZERO.
     03  WK-DENCNTR REDEFINES WK-DENCNT.
         05  WK-DENCNTT       PIC  9(05)  OCCURS 20.
*
 01  WK-TANA-AREA.
     03  WK-TANABAN          PIC  X(06)  VALUE  SPACE.
*
 01  FILE-ERR.
     03  DEN-ERR             PIC  N(10)  VALUE
                   NC"伝票データＦ異常".
     03  JYO-ERR             PIC  N(10)  VALUE
                   NC"条件ファイル異常".
     03  TEN-ERR             PIC  N(10)  VALUE
                   NC"店舗マスタ　異常".
     03  D01-ERR            PIC  N(10)  VALUE
                   NC"手書きＦ１異常".
     03  D02-ERR            PIC  N(10)  VALUE
                   NC"手書きＦ２異常".
     03  D03-ERR            PIC  N(10)  VALUE
                   NC"手書きＦ３異常".
     03  D04-ERR            PIC  N(10)  VALUE
                   NC"手書きＦ４異常".
     03  D05-ERR            PIC  N(10)  VALUE
                   NC"手書きＦ５異常".
     03  D06-ERR            PIC  N(10)  VALUE
                   NC"手書きＦ６異常".
     03  D07-ERR            PIC  N(10)  VALUE
                   NC"手書きＦ７異常".
     03  D08-ERR            PIC  N(10)  VALUE
                   NC"手書きＦ８異常".
     03  D09-ERR            PIC  N(10)  VALUE
                   NC"手書きＦ９異常".
     03  D10-ERR            PIC  N(10)  VALUE
                   NC"手書きＦ１０異常".
     03  D11-ERR            PIC  N(10)  VALUE
                   NC"手書きＦ１１異常".
     03  D12-ERR            PIC  N(10)  VALUE
                   NC"手書きＦ１２異常".
     03  D13-ERR            PIC  N(10)  VALUE
                   NC"手書きＦ１３異常".
     03  D14-ERR            PIC  N(10)  VALUE
                   NC"手書きＦ１４異常".
     03  D15-ERR            PIC  N(10)  VALUE
                   NC"手書きＦ１５異常".
     03  D16-ERR            PIC  N(10)  VALUE
                   NC"手書きＦ１６異常".
     03  D17-ERR            PIC  N(10)  VALUE
                   NC"手書きＦ１７異常".
     03  D18-ERR            PIC  N(10)  VALUE
                   NC"手書きＦ１８異常".
     03  D19-ERR            PIC  N(10)  VALUE
                   NC"手書きＦ１９異常".
     03  D20-ERR            PIC  N(10)  VALUE
                   NC"手書きＦ２０異常".
     03  SHO-ERR            PIC  N(10)  VALUE
                   NC"商品変換２Ｆ異常".
     03  SH1-ERR            PIC  N(10)  VALUE
                   NC"商品変換１Ｆ異常".
*    見出し行１
 01  MIDASHI1.
     03  FILLER                   PIC       X(36)  VALUE SPACE.
     03  FILLER                   PIC       N(19)  VALUE
       NC"【　伝票発行場所確認リスト（手書）　】"
       CHARACTER   TYPE   IS   YB-21.
     03  FILLER                   PIC       X(16)  VALUE SPACE.
     03  FILLER                   PIC       X(05)  VALUE
         "DATE:".
     03  YY                       PIC       Z9.
     03  FILLER                   PIC       X(01)  VALUE
         ".".
     03  MM                       PIC       Z9.
     03  FILLER                   PIC       X(01)  VALUE
         ".".
     03  DD                       PIC       Z9.
     03  FILLER                   PIC       X(01)  VALUE SPACE.
     03  FILLER                   PIC       X(05)  VALUE
         "PAGE:".
     03  PEIJI                    PIC       ZZZ9.
*
*    見出し行２
 01  MIDASHI2           CHARACTER TYPE      IS     YA.
     03  FILLER                   PIC       X(35)  VALUE SPACE.
     03  FILLER                   PIC       N(04)  VALUE
       NC"ＦＬＣＤ".
     03  FILLER                   PIC       X(02)  VALUE SPACE.
     03  FILLER                   PIC       N(04)  VALUE
       NC"場所ＣＤ".
     03  FILLER                   PIC       X(02)  VALUE SPACE.
     03  FILLER                   PIC       N(05)  VALUE
       NC"発行場所名".
     03  FILLER                   PIC       X(12)  VALUE SPACE.
     03  FILLER                   PIC       N(05)  VALUE
       NC"データ件数".
     03  FILLER                   PIC       X(02)  VALUE SPACE.
     03  FILLER                   PIC       N(04)  VALUE
       NC"伝票枚数".
     03  FILLER                   PIC       X(68)  VALUE SPACE.
*
*
*    明細行
 01  MEISAI             CHARACTER TYPE      IS     YA.
     03  FILLER                   PIC       X(36)  VALUE SPACE.
     03  FILECD                   PIC       ZZ,ZZ9.
     03  FILLER                   PIC       X(08)  VALUE SPACE.
     03  ROUTECD                  PIC       9(02).
     03  FILLER                   PIC       X(04)  VALUE SPACE.
     03  ROUTENM                  PIC       N(10).
     03  FILLER                   PIC       X(04)  VALUE SPACE.
     03  DATASU                   PIC       ZZ,ZZ9.
     03  FILLER                   PIC       X(04)  VALUE SPACE.
     03  DENMAI                   PIC       ZZ,ZZ9.
     03  FILLER                   PIC       X(68)  VALUE SPACE.
*    線１
 01  SEN1               CHARACTER TYPE      IS     YA.
     03  FILLER                   PIC       N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                   PIC       N(25)  VALUE
         NC"─────────────────────────".
     03  FILLER                   PIC       N(18)  VALUE
         NC"──────────────────".
*    線２
 01  SEN2.
     03  FILLER                   PIC       X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                   PIC       X(50)  VALUE
         "--------------------------------------------------".
     03  FILLER                   PIC       X(36)  VALUE
         "------------------------------------".
*
****************************************************************
 PROCEDURE                   DIVISION.
****************************************************************
 DECLARATIVES.
 DEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SHTDENF.
     DISPLAY     DEN-ERR     UPON      CONS.
     DISPLAY     DEN-ST      UPON      CONS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 JYO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HJYOKEN.
     DISPLAY     JYO-ERR     UPON      CONS.
     DISPLAY     JYO-ST      UPON      CONS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 TEN-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HTENMS.
     DISPLAY     TEN-ERR     UPON      CONS.
     DISPLAY     TEN-ST      UPON      CONS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 ONL1-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JHTDEN01.
     DISPLAY     D01-ERR     UPON      CONS.
     DISPLAY     D01-ST      UPON      CONS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 ONL2-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JHTDEN02.
     DISPLAY     D02-ERR     UPON      CONS.
     DISPLAY     D02-ST      UPON      CONS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 ONL3-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JHTDEN03.
     DISPLAY     D03-ERR     UPON      CONS.
     DISPLAY     D03-ST      UPON      CONS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 ONL4-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JHTDEN04.
     DISPLAY     D04-ERR     UPON      CONS.
     DISPLAY     D04-ST      UPON      CONS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 ONL5-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JHTDEN05.
     DISPLAY     D05-ERR     UPON      CONS.
     DISPLAY     D05-ST      UPON      CONS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 ONL6-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JHTDEN06.
     DISPLAY     D06-ERR     UPON      CONS.
     DISPLAY     D06-ST      UPON      CONS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 ONL7-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JHTDEN07.
     DISPLAY     D07-ERR     UPON      CONS.
     DISPLAY     D07-ST      UPON      CONS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 ONL8-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JHTDEN08.
     DISPLAY     D08-ERR     UPON      CONS.
     DISPLAY     D08-ST      UPON      CONS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 ONL9-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JHTDEN09.
     DISPLAY     D09-ERR     UPON      CONS.
     DISPLAY     D09-ST      UPON      CONS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 ONL10-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JHTDEN10.
     DISPLAY     D10-ERR     UPON      CONS.
     DISPLAY     D10-ST      UPON      CONS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 ONL11-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JHTDEN11.
     DISPLAY     D11-ERR     UPON      CONS.
     DISPLAY     D11-ST      UPON      CONS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 ONL12-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JHTDEN12.
     DISPLAY     D12-ERR     UPON      CONS.
     DISPLAY     D12-ST      UPON      CONS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 ONL13-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JHTDEN13.
     DISPLAY     D13-ERR     UPON      CONS.
     DISPLAY     D13-ST      UPON      CONS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 ONL14-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JHTDEN14.
     DISPLAY     D14-ERR     UPON      CONS.
     DISPLAY     D14-ST      UPON      CONS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 ONL15-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JHTDEN15.
     DISPLAY     D15-ERR     UPON      CONS.
     DISPLAY     D15-ST      UPON      CONS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 ONL16-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JHTDEN16.
     DISPLAY     D16-ERR     UPON      CONS.
     DISPLAY     D16-ST      UPON      CONS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 ONL17-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JHTDEN17.
     DISPLAY     D17-ERR     UPON      CONS.
     DISPLAY     D17-ST      UPON      CONS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 ONL18-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JHTDEN18.
     DISPLAY     D18-ERR     UPON      CONS.
     DISPLAY     D18-ST      UPON      CONS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 ONL19-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JHTDEN19.
     DISPLAY     D19-ERR     UPON      CONS.
     DISPLAY     D19-ST      UPON      CONS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 ONL20-ERR                    SECTION.
     USE         AFTER       EXCEPTION PROCEDURE JHTDEN20.
     DISPLAY     D20-ERR     UPON      CONS.
     DISPLAY     D20-ST      UPON      CONS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 SHO-ERR                      SECTION.
     USE         AFTER       EXCEPTION PROCEDURE HSHOTBL.
     DISPLAY     SHO-ERR     UPON      CONS.
     DISPLAY     SHO-ST      UPON      CONS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 SH1-ERR                      SECTION.
     USE         AFTER       EXCEPTION PROCEDURE SHOTBL1.
     DISPLAY     SH1-ERR     UPON      CONS.
     DISPLAY     SH1-ST      UPON      CONS.
     ACCEPT      IN-DATA     FROM      CONS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*                 P R O G R A M - S E C
****************************************************************
 PROGRAM-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC    UNTIL     END-FLG  = 9.
     PERFORM     END-SEC.
     STOP        RUN.
*PROGRAM-END.
****************************************************************
*                 I N I T - S E C
****************************************************************
 INIT-SEC                    SECTION.
     OPEN        INPUT       SHTDENF   HJYOKEN   HTENMS
                             HSHOTBL   SHOTBL1
                 OUTPUT      JHTDEN01  JHTDEN02  JHTDEN03
                             JHTDEN04  JHTDEN05  JHTDEN06
                             JHTDEN07  JHTDEN08  JHTDEN09
                             JHTDEN10  JHTDEN11  JHTDEN12
                             JHTDEN13  JHTDEN14  JHTDEN15
                             JHTDEN16  JHTDEN17  JHTDEN18
                             JHTDEN19  JHTDEN20  PRINTF.
*手書き ＲＥＣ初期化
     MOVE        SPACE       TO   D01-REC   D02-REC   D03-REC
                                  D04-REC   D05-REC   D06-REC
                                  D07-REC   D08-REC   D09-REC
                                  D10-REC   D11-REC   D12-REC
                                  D13-REC   D14-REC   D15-REC
                                  D16-REC   D17-REC   D18-REC
                                  D19-REC   D20-REC.
     INITIALIZE                   D01-REC   D02-REC   D03-REC
                                  D04-REC   D05-REC   D06-REC
                                  D07-REC   D08-REC   D09-REC
                                  D10-REC   D11-REC   D12-REC
                                  D13-REC   D14-REC   D15-REC
                                  D16-REC   D17-REC   D18-REC
                                  D19-REC   D20-REC.
     ACCEPT      SYS-DATE   FROM      DATE.
*伝票データ初期読み
     PERFORM  DEN-RD-SEC.
 INIT-EXIT.
     EXIT.
****************************************************************
*                 M A I N - S E C
****************************************************************
 MAIN-SEC                    SECTION.
*データ振り分け
     PERFORM  FURI-SEC.
*伝票データＲＥＡＤ
     PERFORM  DEN-RD-SEC.
 MAIN-EXIT.
     EXIT.
****************************************************************
*               伝 票 デ ー タ ２ Ｒ Ｅ Ａ Ｄ
****************************************************************
 DEN-RD-SEC                 SECTION.
     READ     SHTDENF   AT   END
              MOVE      9         TO   END-FLG
              GO        TO        DEN-RD-EXIT
     END-READ.
*終了条件
*売上作成ＦＬＧ
     IF  DEN-F277            =    9
         MOVE      9         TO   END-FLG
         MOVE      ZERO      TO   SKIP-FLG
         GO             TO   DEN-RD-SEC
     END-IF.
*オンライン
     IF  DEN-F274       NOT  =    0
         MOVE      9         TO   END-FLG
         MOVE      ZERO      TO   SKIP-FLG
         GO             TO   DEN-RD-EXIT
     END-IF.
*読み飛ばし条件
*行_（８０の場合）
*行番号＝８０行が抽出される場合
*    （１）同一伝票番号で売上フラグ（１～７９）が未売上の場合
*行番号＝８０行ば抽出されない場合
*    （１）同一伝票番号で売上フラグ（１～７９）が売上済の場合
*    （２）伝票番号が前データと異なる場合
     IF  DEN-F03        =    80
         IF     DEN-F02  =  WK-DENPYO
                IF   SKIP-FLG  =  ZERO
                     GO      TO   DEN-RD-SEC
                ELSE
                     MOVE ZERO TO SKIP-FLG
                END-IF
         ELSE
                MOVE   ZERO  TO   SKIP-FLG
                GO           TO   DEN-RD-SEC
         END-IF
     END-IF.
*行_（９０の場合）
     IF  DEN-F03        =    90
         MOVE      ZERO      TO   SKIP-FLG
         GO             TO   DEN-RD-SEC
     END-IF.
*相殺区分
     IF  DEN-F04        NOT  =    0
         MOVE      ZERO      TO   SKIP-FLG
         GO             TO   DEN-RD-SEC
     END-IF.
*伝区
     IF  DEN-F051       NOT  =    40
         MOVE      ZERO      TO   SKIP-FLG
         GO             TO   DEN-RD-SEC
     END-IF.
*伝発
     IF  DEN-F134       NOT  =    0
         MOVE      ZERO      TO   SKIP-FLG
         GO             TO   DEN-RD-SEC
     END-IF.
*#2008/08/11 NAV START ##
*承認日
     IF  DEN-F64  =  ZERO
         MOVE      ZERO      TO   SKIP-FLG
         GO             TO   DEN-RD-SEC
     END-IF.
*#2008/08/11 NAV END   ##
*条件ファイルＲＥＡＤ
     IF   (BR-BASHO  NOT =  DEN-F09)
         PERFORM   JYO-RD-SEC
         MOVE      DEN-F09        TO   BR-BASHO
     END-IF.
*
     IF   (INV-SW  =  1)
         MOVE   ZERO    TO   SKIP-FLG
         GO             TO   DEN-RD-SEC
     END-IF.
*振分対象伝票番号退避／売上フラグチェック
     MOVE    DEN-F02    TO   WK-DENPYO.
     MOVE    1          TO   SKIP-FLG.
*
 DEN-RD-EXIT.
     EXIT.
****************************************************************
*               条件ファイル      Ｒ Ｅ Ａ Ｄ
****************************************************************
 JYO-RD-SEC                  SECTION.
     MOVE     SPACE          TO   JYO-F02.
     MOVE     20             TO   JYO-F01.
     MOVE     DEN-F09        TO   JYO-F02.
     READ     HJYOKEN
       INVALID
              MOVE      1         TO   INV-SW
              DISPLAY  "ﾃﾞﾝﾋﾟｮｳ ﾊｯｺｳﾊﾞｼｮ  INV = " DEN-F09
                                  UPON CONS
              ACCEPT    IN-DATA   FROM CONS
       NOT INVALID
              MOVE      ZERO      TO   INV-SW
              MOVE      JYO-F04   TO   WK-BASHO
              EVALUATE  JYO-F04
                 WHEN   1   MOVE  JYO-F04  TO  WK-FLCD1
                            MOVE  DEN-F09  TO  WK-RTCD1
                            MOVE  JYO-F03  TO  WK-RTNM1
                 WHEN   2   MOVE  JYO-F04  TO  WK-FLCD2
                            MOVE  DEN-F09  TO  WK-RTCD2
                            MOVE  JYO-F03  TO  WK-RTNM2
                 WHEN   3   MOVE  JYO-F04  TO  WK-FLCD3
                            MOVE  DEN-F09  TO  WK-RTCD3
                            MOVE  JYO-F03  TO  WK-RTNM3
                 WHEN   4   MOVE  JYO-F04  TO  WK-FLCD4
                            MOVE  DEN-F09  TO  WK-RTCD4
                            MOVE  JYO-F03  TO  WK-RTNM4
                 WHEN   5   MOVE  JYO-F04  TO  WK-FLCD5
                            MOVE  DEN-F09  TO  WK-RTCD5
                            MOVE  JYO-F03  TO  WK-RTNM5
                 WHEN   6   MOVE  JYO-F04  TO  WK-FLCD6
                            MOVE  DEN-F09  TO  WK-RTCD6
                            MOVE  JYO-F03  TO  WK-RTNM6
                 WHEN   7   MOVE  JYO-F04  TO  WK-FLCD7
                            MOVE  DEN-F09  TO  WK-RTCD7
                            MOVE  JYO-F03  TO  WK-RTNM7
                 WHEN   8   MOVE  JYO-F04  TO  WK-FLCD8
                            MOVE  DEN-F09  TO  WK-RTCD8
                            MOVE  JYO-F03  TO  WK-RTNM8
                 WHEN   9   MOVE  JYO-F04  TO  WK-FLCD9
                            MOVE  DEN-F09  TO  WK-RTCD9
                            MOVE  JYO-F03  TO  WK-RTNM9
                 WHEN  10   MOVE  JYO-F04  TO  WK-FLCD10
                            MOVE  DEN-F09  TO  WK-RTCD10
                            MOVE  JYO-F03  TO  WK-RTNM10
                 WHEN  11   MOVE  JYO-F04  TO  WK-FLCD11
                            MOVE  DEN-F09  TO  WK-RTCD11
                            MOVE  JYO-F03  TO  WK-RTNM11
                 WHEN  12   MOVE  JYO-F04  TO  WK-FLCD12
                            MOVE  DEN-F09  TO  WK-RTCD12
                            MOVE  JYO-F03  TO  WK-RTNM12
                 WHEN  13   MOVE  JYO-F04  TO  WK-FLCD13
                            MOVE  DEN-F09  TO  WK-RTCD13
                            MOVE  JYO-F03  TO  WK-RTNM13
                 WHEN  14   MOVE  JYO-F04  TO  WK-FLCD14
                            MOVE  DEN-F09  TO  WK-RTCD14
                            MOVE  JYO-F03  TO  WK-RTNM14
                 WHEN  15   MOVE  JYO-F04  TO  WK-FLCD15
                            MOVE  DEN-F09  TO  WK-RTCD15
                            MOVE  JYO-F03  TO  WK-RTNM15
                 WHEN  16   MOVE  JYO-F04  TO  WK-FLCD16
                            MOVE  DEN-F09  TO  WK-RTCD16
                            MOVE  JYO-F03  TO  WK-RTNM16
                 WHEN  17   MOVE  JYO-F04  TO  WK-FLCD17
                            MOVE  DEN-F09  TO  WK-RTCD17
                            MOVE  JYO-F03  TO  WK-RTNM17
                 WHEN  18   MOVE  JYO-F04  TO  WK-FLCD18
                            MOVE  DEN-F09  TO  WK-RTCD18
                            MOVE  JYO-F03  TO  WK-RTNM18
                 WHEN  19   MOVE  JYO-F04  TO  WK-FLCD19
                            MOVE  DEN-F09  TO  WK-RTCD19
                            MOVE  JYO-F03  TO  WK-RTNM19
                 WHEN  20   MOVE  JYO-F04  TO  WK-FLCD20
                            MOVE  DEN-F09  TO  WK-RTCD20
                            MOVE  JYO-F03  TO  WK-RTNM20
              END-EVALUATE
     END-READ.
 JYO-RD-EXIT.
     EXIT.
****************************************************************
*                 デ ー タ 振 り 分 け
****************************************************************
 FURI-SEC                     SECTION.
*西友殿売場コード取得
     IF        DEN-F01  =  143163
               MOVE  DEN-F01      TO   TEN-F52
               MOVE  DEN-F07      TO   TEN-F011
               MOVE  "CHK"        TO   CHK-FLG
               PERFORM  TEN-RD-SEC
               IF     TEN-INV-FLG  =  "INV"
                      MOVE  SPACE TO   CHK-FLG
               ELSE
                DISPLAY "TEN-F79 = " TEN-F79 UPON CONS
                MOVE     TEN-F79  TO   WK-URIBA
               END-IF
     ELSE
               MOVE  SPACE        TO   CHK-FLG
     END-IF.
*ケーヨー（新）情報取得
*2000/05/22 カーマ追加
     IF        DEN-F01  =  173  OR  13938
               MOVE  DEN-F01      TO   TEN-F52
               MOVE  DEN-F07      TO   TEN-F011
               MOVE  "CHK"        TO   CHK1-FLG
               PERFORM  TEN-RD-SEC
               IF     TEN-INV-FLG  =  "INV"
                      MOVE  SPACE TO   CHK1-FLG
               ELSE
                      IF    TEN-F79  NOT =  1
                            MOVE SPACE TO CHK1-FLG
                      END-IF
               END-IF
     ELSE
               MOVE  SPACE        TO   CHK1-FLG
     END-IF.
*_番取得処理
     PERFORM   TANABAN-GET-SEC.
*データ振り分け処理
     EVALUATE  WK-BASHO
       WHEN     1
               MOVE     DEN-REC   TO   D01-REC
               IF       CHK-FLG = "CHK"
                        MOVE TEN-F79  TO D01-REC2
               END-IF
               IF       CHK1-FLG = "CHK"
                        MOVE 1        TO D01-REC15
               END-IF
               MOVE     WK-TANABAN    TO D01-REC3(112:6)
               MOVE     ZERO          TO D01-REC3(110:2)
               WRITE    D01-REC
               END-WRITE
               ADD      1         TO   WK-RTCNT1
               IF       DEN-F02   NOT =  WK-DENNO
                        ADD       1         TO   WK-DENCNT1
                        MOVE      DEN-F02   TO   WK-DENNO
               END-IF
       WHEN     2
               MOVE     DEN-REC   TO   D02-REC
               IF       CHK-FLG = "CHK"
                        MOVE TEN-F79  TO D02-REC2
               END-IF
               IF       CHK1-FLG = "CHK"
                        MOVE 1        TO D02-REC15
               END-IF
               MOVE     WK-TANABAN    TO D02-REC3(112:6)
               MOVE     ZERO          TO D02-REC3(110:2)
               WRITE    D02-REC
               END-WRITE
               ADD      1         TO   WK-RTCNT2
               IF       DEN-F02   NOT =  WK-DENNO
                        ADD       1         TO   WK-DENCNT2
                        MOVE      DEN-F02   TO   WK-DENNO
               END-IF
       WHEN     3
               MOVE     DEN-REC   TO   D03-REC
               IF       CHK-FLG = "CHK"
                        MOVE TEN-F79  TO D03-REC2
               END-IF
               IF       CHK1-FLG = "CHK"
                        MOVE 1        TO D03-REC15
               END-IF
               MOVE     WK-TANABAN    TO D03-REC3(112:6)
               MOVE     ZERO          TO D03-REC3(110:2)
               WRITE    D03-REC
               END-WRITE
               ADD      1         TO   WK-RTCNT3
               IF       DEN-F02   NOT =  WK-DENNO
                        ADD       1         TO   WK-DENCNT3
                        MOVE      DEN-F02   TO   WK-DENNO
               END-IF
       WHEN     4
               MOVE     DEN-REC   TO   D04-REC
               IF       CHK-FLG = "CHK"
                        MOVE TEN-F79  TO D04-REC2
               END-IF
               IF       CHK1-FLG = "CHK"
                        MOVE 1        TO D04-REC15
               END-IF
               MOVE     WK-TANABAN    TO D04-REC3(112:6)
               MOVE     ZERO          TO D04-REC3(110:2)
               WRITE    D04-REC
               END-WRITE
               ADD      1         TO   WK-RTCNT4
               IF       DEN-F02   NOT =  WK-DENNO
                        ADD       1         TO   WK-DENCNT4
                        MOVE      DEN-F02   TO   WK-DENNO
               END-IF
       WHEN     5
               MOVE     DEN-REC   TO   D05-REC
               IF       CHK-FLG = "CHK"
                        MOVE TEN-F79  TO D05-REC2
               END-IF
               IF       CHK1-FLG = "CHK"
                        MOVE 1        TO D05-REC15
               END-IF
               MOVE     WK-TANABAN    TO D05-REC3(112:6)
               MOVE     ZERO          TO D05-REC3(110:2)
               WRITE    D05-REC
               END-WRITE
               ADD      1         TO   WK-RTCNT5
               IF       DEN-F02   NOT =  WK-DENNO
                        ADD       1         TO   WK-DENCNT5
                        MOVE      DEN-F02   TO   WK-DENNO
               END-IF
       WHEN     6
               MOVE     DEN-REC   TO   D06-REC
               IF       CHK-FLG = "CHK"
                        MOVE TEN-F79  TO D06-REC2
               END-IF
               IF       CHK1-FLG = "CHK"
                        MOVE 1        TO D06-REC15
               END-IF
               MOVE     WK-TANABAN    TO D06-REC3(112:6)
               MOVE     ZERO          TO D06-REC3(110:2)
               WRITE    D06-REC
               END-WRITE
               ADD      1         TO   WK-RTCNT6
               IF       DEN-F02   NOT =  WK-DENNO
                        ADD       1         TO   WK-DENCNT6
                        MOVE      DEN-F02   TO   WK-DENNO
               END-IF
       WHEN     7
               MOVE     DEN-REC   TO   D07-REC
               IF       CHK-FLG = "CHK"
                        MOVE TEN-F79  TO D07-REC2
               END-IF
               IF       CHK1-FLG = "CHK"
                        MOVE 1        TO D07-REC15
               END-IF
               MOVE     WK-TANABAN    TO D07-REC3(112:6)
               MOVE     ZERO          TO D07-REC3(110:2)
               WRITE    D07-REC
               END-WRITE
               ADD      1         TO   WK-RTCNT7
               IF       DEN-F02   NOT =  WK-DENNO
                        ADD       1         TO   WK-DENCNT7
                        MOVE      DEN-F02   TO   WK-DENNO
               END-IF
       WHEN     8
               MOVE     DEN-REC   TO   D08-REC
               IF       CHK-FLG = "CHK"
                        MOVE TEN-F79  TO D08-REC2
               END-IF
               IF       CHK1-FLG = "CHK"
                        MOVE 1        TO D08-REC15
               END-IF
               MOVE     WK-TANABAN    TO D08-REC3(112:6)
               MOVE     ZERO          TO D08-REC3(110:2)
               WRITE    D08-REC
               END-WRITE
               ADD      1         TO   WK-RTCNT8
               IF       DEN-F02   NOT =  WK-DENNO
                        ADD       1         TO   WK-DENCNT8
                        MOVE      DEN-F02   TO   WK-DENNO
               END-IF
       WHEN     9
               MOVE     DEN-REC   TO   D09-REC
               IF       CHK-FLG = "CHK"
                        MOVE TEN-F79  TO D09-REC2
               END-IF
               IF       CHK1-FLG = "CHK"
                        MOVE 1        TO D09-REC15
               END-IF
               MOVE     WK-TANABAN    TO D09-REC3(112:6)
               MOVE     ZERO          TO D09-REC3(110:2)
               WRITE    D09-REC
               END-WRITE
               ADD      1         TO   WK-RTCNT9
               IF       DEN-F02   NOT =  WK-DENNO
                        ADD       1         TO   WK-DENCNT9
                        MOVE      DEN-F02   TO   WK-DENNO
               END-IF
       WHEN    10
               MOVE     DEN-REC   TO   D10-REC
               IF       CHK-FLG = "CHK"
                        MOVE TEN-F79  TO D10-REC2
               END-IF
               IF       CHK1-FLG = "CHK"
                        MOVE 1        TO D10-REC15
               END-IF
               MOVE     WK-TANABAN    TO D10-REC3(112:6)
               MOVE     ZERO          TO D10-REC3(110:2)
               WRITE    D10-REC
               END-WRITE
               ADD      1         TO   WK-RTCNT10
               IF       DEN-F02   NOT =  WK-DENNO
                        ADD       1         TO   WK-DENCNT10
                        MOVE      DEN-F02   TO   WK-DENNO
               END-IF
       WHEN    11
               MOVE     DEN-REC   TO   D11-REC
               IF       CHK-FLG = "CHK"
                        MOVE TEN-F79  TO D11-REC2
               END-IF
               IF       CHK1-FLG = "CHK"
                        MOVE 1        TO D11-REC15
               END-IF
               MOVE     WK-TANABAN    TO D11-REC3(112:6)
               MOVE     ZERO          TO D11-REC3(110:2)
               WRITE    D11-REC
               END-WRITE
               ADD      1         TO   WK-RTCNT11
               IF       DEN-F02   NOT =  WK-DENNO
                        ADD       1         TO   WK-DENCNT11
                        MOVE      DEN-F02   TO   WK-DENNO
               END-IF
       WHEN    12
               MOVE     DEN-REC   TO   D12-REC
               IF       CHK-FLG = "CHK"
                        MOVE TEN-F79  TO D12-REC2
               END-IF
               IF       CHK1-FLG = "CHK"
                        MOVE 1        TO D12-REC15
               END-IF
               MOVE     WK-TANABAN    TO D12-REC3(112:6)
               MOVE     ZERO          TO D12-REC3(110:2)
               WRITE    D12-REC
               END-WRITE
               ADD      1         TO   WK-RTCNT12
               IF       DEN-F02   NOT =  WK-DENNO
                        ADD       1         TO   WK-DENCNT12
                        MOVE      DEN-F02   TO   WK-DENNO
               END-IF
       WHEN    13
               MOVE     DEN-REC   TO   D13-REC
               IF       CHK-FLG = "CHK"
                        MOVE TEN-F79  TO D13-REC2
               END-IF
               IF       CHK1-FLG = "CHK"
                        MOVE 1        TO D13-REC15
               END-IF
               MOVE     WK-TANABAN    TO D13-REC3(112:6)
               MOVE     ZERO          TO D13-REC3(110:2)
               WRITE    D13-REC
               END-WRITE
               ADD      1         TO   WK-RTCNT13
               IF       DEN-F02   NOT =  WK-DENNO
                        ADD       1         TO   WK-DENCNT13
                        MOVE      DEN-F02   TO   WK-DENNO
               END-IF
       WHEN    14
               MOVE     DEN-REC   TO   D14-REC
               IF       CHK-FLG = "CHK"
                        MOVE TEN-F79  TO D14-REC2
               END-IF
               IF       CHK1-FLG = "CHK"
                        MOVE 1        TO D14-REC15
               END-IF
               MOVE     WK-TANABAN    TO D14-REC3(112:6)
               MOVE     ZERO          TO D14-REC3(110:2)
               WRITE    D14-REC
               END-WRITE
               ADD      1         TO   WK-RTCNT14
               IF       DEN-F02   NOT =  WK-DENNO
                        ADD       1         TO   WK-DENCNT14
                        MOVE      DEN-F02   TO   WK-DENNO
               END-IF
       WHEN    15
               MOVE     DEN-REC   TO   D15-REC
               IF       CHK-FLG = "CHK"
                        MOVE TEN-F79  TO D15-REC2
               END-IF
               IF       CHK1-FLG = "CHK"
                        MOVE 1        TO D15-REC15
               END-IF
               MOVE     WK-TANABAN    TO D15-REC3(112:6)
               MOVE     ZERO          TO D15-REC3(110:2)
               WRITE    D15-REC
               END-WRITE
               ADD      1         TO   WK-RTCNT15
               IF       DEN-F02   NOT =  WK-DENNO
                        ADD       1         TO   WK-DENCNT15
                        MOVE      DEN-F02   TO   WK-DENNO
               END-IF
       WHEN    16
               MOVE     DEN-REC   TO   D16-REC
               IF       CHK-FLG = "CHK"
                        MOVE TEN-F79  TO D16-REC2
               END-IF
               IF       CHK1-FLG = "CHK"
                        MOVE 1        TO D16-REC15
               END-IF
               MOVE     WK-TANABAN    TO D16-REC3(112:6)
               MOVE     ZERO          TO D16-REC3(110:2)
               WRITE    D16-REC
               END-WRITE
               ADD      1         TO   WK-RTCNT16
               IF       DEN-F02   NOT =  WK-DENNO
                        ADD       1         TO   WK-DENCNT16
                        MOVE      DEN-F02   TO   WK-DENNO
               END-IF
       WHEN    17
               MOVE     DEN-REC   TO   D17-REC
               IF       CHK-FLG = "CHK"
                        MOVE TEN-F79  TO D17-REC2
               END-IF
               IF       CHK1-FLG = "CHK"
                        MOVE 1        TO D17-REC15
               END-IF
               MOVE     WK-TANABAN    TO D17-REC3(112:6)
               MOVE     ZERO          TO D17-REC3(110:2)
               WRITE    D17-REC
               END-WRITE
               ADD      1         TO   WK-RTCNT17
               IF       DEN-F02   NOT =  WK-DENNO
                        ADD       1         TO   WK-DENCNT17
                        MOVE      DEN-F02   TO   WK-DENNO
               END-IF
       WHEN    18
               MOVE     DEN-REC   TO   D18-REC
               IF       CHK-FLG = "CHK"
                        MOVE TEN-F79  TO D18-REC2
               END-IF
               IF       CHK1-FLG = "CHK"
                        MOVE 1        TO D18-REC15
               END-IF
               MOVE     WK-TANABAN    TO D18-REC3(112:6)
               MOVE     ZERO          TO D18-REC3(110:2)
               WRITE    D18-REC
               END-WRITE
               ADD      1         TO   WK-RTCNT18
               IF       DEN-F02   NOT =  WK-DENNO
                        ADD       1         TO   WK-DENCNT18
                        MOVE      DEN-F02   TO   WK-DENNO
               END-IF
       WHEN    19
               MOVE     DEN-REC   TO   D19-REC
               IF       CHK-FLG = "CHK"
                        MOVE TEN-F79  TO D19-REC2
               END-IF
               IF       CHK1-FLG = "CHK"
                        MOVE 1        TO D19-REC15
               END-IF
               MOVE     WK-TANABAN    TO D19-REC3(112:6)
               MOVE     ZERO          TO D19-REC3(110:2)
               WRITE    D19-REC
               END-WRITE
               ADD      1         TO   WK-RTCNT19
               IF       DEN-F02   NOT =  WK-DENNO
                        ADD       1         TO   WK-DENCNT19
                        MOVE      DEN-F02   TO   WK-DENNO
               END-IF
       WHEN    20
               MOVE     DEN-REC   TO   D20-REC
               IF       CHK-FLG = "CHK"
                        MOVE TEN-F79  TO D20-REC2
               END-IF
               IF       CHK1-FLG = "CHK"
                        MOVE 1        TO D20-REC15
               END-IF
               MOVE     WK-TANABAN    TO D20-REC3(112:6)
               MOVE     ZERO          TO D20-REC3(110:2)
               WRITE    D20-REC
               END-WRITE
               ADD      1         TO   WK-RTCNT20
               IF       DEN-F02   NOT =  WK-DENNO
                        ADD       1         TO   WK-DENCNT20
                        MOVE      DEN-F02   TO   WK-DENNO
               END-IF
     END-EVALUATE.
 FURI-EXIT.
     EXIT.
****************************************************************
*    終了
****************************************************************
 END-SEC                   SECTION.
     PERFORM  LISTWT-SEC.
     CLOSE    SHTDENF   HJYOKEN   JHTDEN01   JHTDEN02
              JHTDEN03  JHTDEN04  JHTDEN05   JHTDEN06
              JHTDEN07  JHTDEN08  JHTDEN09   JHTDEN10
              JHTDEN11  JHTDEN12  JHTDEN13   JHTDEN14
              JHTDEN15  JHTDEN16  JHTDEN17   JHTDEN18
              JHTDEN19  JHTDEN20
              PRINTF.
 END-EXIT.
     EXIT.
****************************************************************
*           店舗マスタ読込み                        3.1.1      *
****************************************************************
 TEN-RD-SEC   SECTION.
*
     READ     HTENMS     INVALID
              MOVE    "INV"   TO   TEN-INV-FLG
              NOT   INVALID
              MOVE     SPACE  TO   TEN-INV-FLG
     END-READ.
*
 TEN-RD-EXIT.
     EXIT.
****************************************************************
*           リスト出力処理                          3.1.1      *
****************************************************************
 LISTWT-SEC   SECTION.
*
     MOVE      SYS-YY         TO        YY.
     MOVE      SYS-MM         TO        MM.
     MOVE      SYS-DD         TO        DD.
     MOVE      1              TO        PEIJI.
     WRITE     PRINT-REC      FROM      MIDASHI1 AFTER 2.
     WRITE     PRINT-REC      FROM      SEN1     AFTER 2.
     WRITE     PRINT-REC      FROM      MIDASHI2 AFTER 1.
     WRITE     PRINT-REC      FROM      SEN1     AFTER 1.
 LISTWT-010.
     PERFORM   VARYING   IX   FROM      1  BY  1
               UNTIL     IX    >        20
               MOVE      WK-FLCDT(IX)   TO       FILECD
               MOVE      WK-RTNMT(IX)   TO       ROUTENM
               MOVE      WK-RTCDT(IX)   TO       ROUTECD
               MOVE      WK-RTCNTT(IX)  TO       DATASU
               MOVE      WK-DENCNTT(IX) TO       DENMAI
         IF    WK-FLCDT(IX)   NOT =     ZERO
               WRITE     PRINT-REC FROM MEISAI   AFTER 1
               WRITE     PRINT-REC FROM SEN2     AFTER 1
         END-IF
     END-PERFORM.
 LISTWT-EXIT.
     EXIT.
****************************************************************
*           _番取得処理                                       *
****************************************************************
 TANABAN-GET-SEC       SECTION.
*
     MOVE      SPACE            TO    WK-TANABAN.
*_番取得（商品変換テーブル）
     IF       DEN-F25   NOT   =    SPACE
              MOVE    DEN-F01         TO   SH1-F01
              MOVE    DEN-F25         TO   SH1-F02
              READ    SHOTBL1
                      INVALID   KEY
                      MOVE    SPACE   TO   WK-TANABAN
                      NOT INVALID    KEY
                      MOVE    SH1-F08 TO   WK-TANABAN
                      GO              TO   TANABAN-GET-EXIT
             END-READ
     END-IF.
*
     MOVE    DEN-F01         TO   SHO-F01.
     MOVE    DEN-F08         TO   SHO-F04
     MOVE    DEN-F1411       TO   SHO-F031
     MOVE    DEN-F1412       TO   SHO-F032
     READ    HSHOTBL
           INVALID   KEY
             MOVE    SPACE   TO   WK-TANABAN
           NOT INVALID    KEY
             MOVE    SHO-F08 TO   WK-TANABAN
     END-READ.
*
 TANABAN-GET-EXIT.
     EXIT.
********************<<  PROGRAM  END  >>**************************

```

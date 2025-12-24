# NVS0100B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVS0100B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　D365送受信実行制御ファイル作成　
*    作成日／作成者　　　：　2020/02/24   ASS.II               *
*    処理内容　　　　　　：　送受信対象のグループ内訳毎に　　　*
*    　　　　　　　　　　　　制御ファイルレコードを出力する。　*
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           NVS0100B.
 AUTHOR.               ASS.II.
 DATE-WRITTEN.         20/02/24.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*D365送受信グループマスタ
     SELECT   DNGROPF1 ASSIGN    TO        DA-01-VI-DNGROPL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       GRP-F01
                       FILE  STATUS    IS  GRP-ST.
*
*D365送受信データ種別マスタ
     SELECT   DNDATSL1 ASSIGN    TO        DA-01-VI-DNDATSL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       DAT-F01
                       FILE  STATUS    IS  DAT-ST.
*条件ファイル
     SELECT   JYOKEN1        ASSIGN        TO  DA-01-VI-JYOKEN1
                             ORGANIZATION  IS  INDEXED
                             ACCESS MODE   IS  RANDOM
                             RECORD KEY    IS  JYO-F01
                                               JYO-F02
                             FILE STATUS   IS  JYO-ST.
*
*D365送受信実行制御ファイル
     SELECT  D365RUNF  ASSIGN    TO        DA-01-S-D365RUNF
                       ORGANIZATION        SEQUENTIAL
                       ACCESS    MODE      SEQUENTIAL
                       FILE      STATUS    RUN-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
******************************************************************
*    D365送受信グループマスタ
******************************************************************
 FD  DNGROPF1          LABEL     RECORD    IS   STANDARD.
                       COPY      DNGROPF   OF   XFDLIB
                       JOINING   GRP       AS   PREFIX.
*
******************************************************************
*    D365送受信データ種別マスタ
******************************************************************
 FD  DNDATSL1          LABEL     RECORD    IS   STANDARD.
                       COPY      DNDATSF   OF   XFDLIB
                       JOINING   DAT       AS   PREFIX.
*
****************************************************************
*    条件ファイル                               *
****************************************************************
 FD  JYOKEN1.
     COPY     HJYOKEN  OF  XFDLIB
     JOINING  JYO      AS  PREFIX.
*
****************************************************************
*    D365送受信実行制御ファイル                         *
****************************************************************
 FD  D365RUNF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      D365RUNF  OF   XFDLIB
                       JOINING   RUN       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  GRP-ST                   PIC  X(02).
     03  DAT-ST                   PIC  X(02).
     03  JYO-ST                   PIC  X(02).
     03  RUN-ST                   PIC  X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  GRP-ERR           PIC N(15) VALUE
         NC"送受信グループマスタエラー".
     03  DAT-ERR           PIC N(15) VALUE
         NC"送受信データ種別マスタエラー".
     03  JYO-ERR           PIC N(15) VALUE
         NC"条件ファイルエラー".
     03  RUN-ERR           PIC N(15) VALUE
         NC"送受信実行制御ファイルエラー".
*読込フラグ領域
 01  FLG-AREA.
     03  EDI-FLG           PIC  X(01) VALUE SPACE.
     03  KAI-FLG           PIC  X(01) VALUE SPACE.
     03  DAT-INV-FLG       PIC  X(03) VALUE SPACE.
     03  JYO-INV-FLG       PIC  X(03) VALUE SPACE.
*読込・書込カウント領域
 01  CNT-AREA.
     03  DAT-CNT           PIC  9(02) VALUE ZERO.
     03  GRP-CNT           PIC  9(02) VALUE ZERO.
     03  WRITE-CNT         PIC  9(02) VALUE ZERO.
 01  WORK-AREA.
     03  WK-DATCNT         PIC  9(03) VALUE ZERO.
     03  WK-SEGKBN         PIC  X(01) VALUE SPACE.
*データ種別チェックテーブル
 01  DAT-CHK-TABLE.
     03  DAT-CHK           PIC  X(02) OCCURS  5.
*データ種別チェックテーブル
 01  INDEX-AREA.
     03  IX1               PIC  9(03) PACKED-DECIMAL.
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
*------------------------------------------------------------*
 LINKAGE              SECTION.
*------------------------------------------------------------*
*パラメタ取得
 01  LINK-JDATE            PIC 9(08).
 01  LINK-JTIME            PIC 9(06).
 01  LINK-DNGRP            PIC 9(02).
 01  LINK-RUNNO            PIC 9(07).
 01  LINK-RUNKBN           PIC X(01).
 01  LINK-RUNTNT           PIC X(02).
 01  LINK-DATCNT           PIC 9(03).
 01  LINK-SEGKBN           PIC X(01).
*
**************************************************************
 PROCEDURE             DIVISION   USING    LINK-JDATE
                                           LINK-JTIME
                                           LINK-DNGRP
                                           LINK-RUNNO
                                           LINK-RUNKBN
                                           LINK-RUNTNT
                                           LINK-DATCNT
                                           LINK-SEGKBN.
**************************************************************
 DECLARATIVES.
 GRP-ERR-SEC               SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DNGROPF1.
     MOVE        GRP-ST    TO        E-ST.
     MOVE       "DNGROPF1" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     GRP-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 DAT-ERR-SEC               SECTION.
     USE         AFTER     EXCEPTION PROCEDURE DNDATSL1.
     MOVE        DAT-ST    TO        E-ST.
     MOVE       "DNDATSL1" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     DAT-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 JYO-ERR-SEC               SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JYOKEN1.
     MOVE        JYO-ST    TO        E-ST.
     MOVE        "JYOKEN1" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     JYO-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 RUN-ERR-SEC               SECTION.
     USE         AFTER     EXCEPTION PROCEDURE D365RUNF.
     MOVE        RUN-ST    TO        E-ST.
     MOVE       "D365RUNF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     RUN-ERR   UPON      CONS.
     MOVE         "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
**************************************
**   MOVE  "20200201" TO       LINK-JDATE  .
**   MOVE  "120101"   TO       LINK-JTIME  .
**   MOVE  "01"       TO       LINK-DNGRP  .
**   MOVE  "1000020"  TO       LINK-RUNNO  .
**   MOVE  "1"        TO       LINK-RUNKBN .
**   MOVE  "22"       TO       LINK-RUNTNT .
*
     DISPLAY "日付          ="  LINK-JDATE UPON CONS.
     DISPLAY "時刻          ="  LINK-JTIME UPON CONS.
     DISPLAY "送受信グループ="  LINK-DNGRP  UPON CONS.
     DISPLAY "実行ＮＯ      ="  LINK-RUNNO  UPON CONS.
     DISPLAY "実行区分      ="  LINK-RUNKBN  UPON CONS.
     DISPLAY "実行担当者ＣＤ="  LINK-RUNTNT  UPON CONS.
**************************************
*
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC.
     PERFORM   END-SEC.
     STOP  RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               0.0       *
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"     TO   S-NAME.
*ファイルのＯＰＥＮ
     OPEN      INPUT   DNGROPF1   DNDATSL1.
     OPEN      I-O     JYOKEN1.
     OPEN      OUTPUT  D365RUNF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*送受信グループマスタタのキー項目を設定
     MOVE      LINK-DNGRP    TO   GRP-F01.
*送受信グループマスタタの読込
     READ      DNGROPF1
               INVALID
                  MOVE "1"    TO   LINK-SEGKBN
                  GO          TO   MAIN-EXIT
               NOT INVALID
                  ADD   1     TO   GRP-CNT
     END-READ.
*データ種別チェック
     IF        GRP-F03       =     SPACE  AND
               GRP-F04       =     SPACE  AND
               GRP-F05       =     SPACE  AND
               GRP-F06       =     SPACE  AND
               GRP-F07       =     SPACE
               MOVE "1"      TO    WK-SEGKBN
               GO            TO    MAIN-EXIT
      END-IF.
*
      MOVE     SPACE         TO    DAT-CHK-TABLE.
      MOVE     GRP-F03       TO    DAT-CHK(1).
      MOVE     GRP-F04       TO    DAT-CHK(2).
      MOVE     GRP-F05       TO    DAT-CHK(3).
      MOVE     GRP-F06       TO    DAT-CHK(4).
      MOVE     GRP-F07       TO    DAT-CHK(5).
      MOVE     SPACE         TO    WK-SEGKBN.

      PERFORM  VARYING IX1 FROM 1 BY 1  UNTIL IX1 > 5
         IF    DAT-CHK(IX1)  =    SPACE
               CONTINUE
         ELSE
               MOVE  DAT-CHK(IX1)  TO  DAT-F01
               PERFORM       DNDATSL1-READ-SEC
               IF    DAT-INV-FLG   =  "INV"
                     MOVE  "1"       TO  WK-SEGKBN
                     MOVE  99        TO  IX1
               ELSE
                     PERFORM       JYOKEN1-READ-SEC
                     IF   JYO-INV-FLG  =  "INV"
                          MOVE  "1"       TO  WK-SEGKBN
                          MOVE  99        TO  IX1
                     ELSE
*条件ファイル更新
                          ADD    1        TO  JYO-F04
                          IF     JYO-F04  >   JYO-F06
                                 MOVE  JYO-F05  TO  JYO-F04
                          END-IF
                          REWRITE   JYO-REC
                          PERFORM  D365RUNF-OUT-SEC
                     END-IF
               END-IF
         END-IF
      END-PERFORM.

*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"      TO   S-NAME.
*
     MOVE      WRITE-CNT     TO   LINK-DATCNT.
     MOVE      WK-SEGKBN     TO   LINK-SEGKBN.
*ファイルのＯＰＥＮ
     CLOSE     DNGROPF1   DNDATSL1 JYOKEN1 D365RUNF.
***************************
     DISPLAY "データ作成件数=" WRITE-CNT  UPON CONS.
     DISPLAY "制御区分      =" WK-SEGKBN  UPON CONS.
***************************
*
 END-EXIT.
     EXIT.
****************************************************************
*             送受信データ種別マスタ 読込み          2.1
****************************************************************
 DNDATSL1-READ-SEC      SECTION.
     MOVE     "DNDATSL1-READ-SEC"   TO   S-NAME.
*実行制御マスタ読込み
     READ      DNDATSL1  INVALID
               MOVE     "INV"      TO   DAT-INV-FLG
               NOT  INVALID
               MOVE     SPACE      TO   DAT-INV-FLG
     END-READ.
*
 DNDATSL1-READ-EXIT.
     EXIT.
****************************************************************
*             条件マスタ読込み                        2.2      *
****************************************************************
 JYOKEN1-READ-SEC      SECTION.
     MOVE     "JYOKEN1-READ-SEC"   TO   S-NAME.
*条件マスタ読込み
     MOVE      "99"                TO   JYO-F01.
     MOVE      "D36501"            TO   JYO-F02.
     READ      JYOKEN1    INVALID
               MOVE     "INV"      TO   JYO-INV-FLG
               NOT  INVALID
               MOVE     SPACE      TO   JYO-INV-FLG
     END-READ.
*
 JYOKEN1-READ-EXIT.
     EXIT.
****************************************************************
*             送受信実行制御ファイル出力              2.3      *
****************************************************************
 D365RUNF-OUT-SEC      SECTION.
     MOVE     "D365RUNF-OUT-SEC"   TO   S-NAME.
*送受信実行制御レコード編集
     MOVE      SPACE               TO   RUN-REC.
     INITIALIZE                         RUN-REC.
     MOVE      LINK-RUNNO          TO   RUN-F01.
     MOVE      JYO-F04             TO   RUN-F02.
     MOVE      LINK-RUNKBN         TO   RUN-F03.
     MOVE      DAT-F03             TO   RUN-F04.
     MOVE      DAT-F01             TO   RUN-F05.
     MOVE      LINK-JDATE          TO   RUN-F06.
     MOVE      LINK-JTIME          TO   RUN-F07.
     MOVE      DAT-F04             TO   RUN-F08.
     MOVE      DAT-F05             TO   RUN-F09.
     MOVE      DAT-F06             TO   RUN-F10.
     MOVE      DAT-F07             TO   RUN-F11.
     MOVE      DAT-F08             TO   RUN-F12.
     MOVE      DAT-F09             TO   RUN-F13.
     MOVE      LINK-RUNTNT         TO   RUN-F14.
*送受信実行制御レコード出力
     WRITE     RUN-REC.
     ADD       1                    TO   WRITE-CNT.
*
 D365RUNF-OUT-EXIT.
     EXIT.
*****************<<  NVS0100B   END PROGRAM  >>******************

```

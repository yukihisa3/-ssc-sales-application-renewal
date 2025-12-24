# SEG0070B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SEG0070B.COB`

## ソースコード

```cobol
****************************************************************
*    （大阪営業所－全データ抽出バージョン）                    *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＣＶＣＳ管理                      *
*    モジュール名　　　　：　受信件数マスタ更新                *
*    作成日／更新日　　　：　2000/03/28                        *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　受信した受信件数マスタを営業所受 *
*                            信件数マスタへ更新スル。         *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SEG0070B.
 AUTHOR.               NAV.
 DATE-WRITTEN.         00/03/28.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*受信件数マスタワーク
     SELECT  JHMKENWK  ASSIGN    TO        DA-01-S-JHMKENWK
                       FILE      STATUS    KEW-ST.
*受信件数マスタ
     SELECT  JHMKENF   ASSIGN    TO        DA-01-VI-JHMKENL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       KEN-F01
                                           KEN-F02
                                           KEN-F03
                                           KEN-F04
                       FILE      STATUS    KEN-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 受信件数マスタワーク                               *
****************************************************************
 FD  JHMKENWK
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JHMKENF   OF   XFDLIB
                       JOINING   KEW       AS   PREFIX.
*
****************************************************************
*    FILE = 受信件数マスタ                                     *
****************************************************************
 FD  JHMKENF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JHMKENF   OF   XFDLIB
                       JOINING   KEN       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  KEW-ST                   PIC  X(02).
     03  KEN-ST                   PIC  X(02).
*時間編集領域
 01  WK-TIME.
     03  WK-TIME-HHMM             PIC  9(04)  VALUE  ZERO.
     03  WK-TIME-SS               PIC  9(04)  VALUE  ZERO.
*フラグ領域
 01  WK-FLG.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
     03  JHMKENF-INV-FLG          PIC  X(03)  VALUE  SPACE.
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  KEW-ERR           PIC N(15) VALUE
         NC"受信件数マスタＷエラー".
     03  KEN-ERR           PIC N(15) VALUE
         NC"受信件数マスタエラー".
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
*
**************************************************************
 PROCEDURE             DIVISION.
**************************************************************
 DECLARATIVES.
 KEW-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMKENWK.
     MOVE        KEW-ST    TO        E-ST.
     MOVE        "JHMKENWK" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     KEW-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 KEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMKENF.
     MOVE        KEN-ST    TO        E-ST.
     MOVE        "JHMKENF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     KEN-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC  UNTIL  END-FLG = "END".
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
     OPEN      INPUT   JHMKENWK.
     OPEN      I-O     JHMKENF.
*受信－受信件数マスタ初期読込み
     PERFORM JHMKENWK-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*ファイルＫＥＹ項目セット
     MOVE      KEW-F01       TO   KEN-F01.
     MOVE      KEW-F02       TO   KEN-F02.
     MOVE      KEW-F03       TO   KEN-F03.
     MOVE      KEW-F04       TO   KEN-F04.
*ファイル読込み
     READ      JHMKENF
               INVALID
                 MOVE  "INV"   TO   JHMKENF-INV-FLG
               NOT  INVALID
                 MOVE  SPACE   TO   JHMKENF-INV-FLG
     END-READ.
*処理区分により各項目更新
     IF   JHMKENF-INV-FLG  =  "INV"
          MOVE SPACE         TO  KEN-REC
          INITIALIZE             KEN-REC
          MOVE KEW-REC       TO  KEN-REC
          WRITE KEN-REC
     ELSE
          MOVE KEW-F05       TO  KEN-F05
          MOVE KEW-F06       TO  KEN-F06
          MOVE KEW-F07       TO  KEN-F07
          MOVE KEW-F08       TO  KEN-F08
          MOVE KEW-F09       TO  KEN-F09
          MOVE KEW-F10       TO  KEN-F10
          MOVE KEW-F11       TO  KEN-F11
          REWRITE KEN-REC
     END-IF.
*受信－受信件数マスタ読込み
     PERFORM JHMKENWK-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 JHMKENWK-READ-SEC     SECTION.
     MOVE "JHMKENF-READ-SEC" TO   S-NAME.
*受信受信件数マスタ読込み
     READ      JHMKENWK  AT  END
               MOVE    "END"    TO   END-FLG
     END-READ.
*
 JHMKENWK-READ-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"      TO   S-NAME.
*ファイルのＯＰＥＮ
     CLOSE     JHMKENWK JHMKENF.
*
 END-EXIT.
     EXIT.
*****************<<  SEG0070B   END PROGRAM  >>******************

```

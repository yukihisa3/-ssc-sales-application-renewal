# SEG0040B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SEG0040B.COB`

## ソースコード

```cobol
****************************************************************
*    （大阪営業所－全データ抽出バージョン）                    *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＣＶＣＳ管理                      *
*    モジュール名　　　　：　受信件数マスタ抽出                *
*    作成日／更新日　　　：　2000/03/28                        *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　パラメタより受け取ったバッチ_の　*
*                            受信件数マスタを抽出する。        *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SEG0040B.
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
*受信件数マスタ
     SELECT  JHMKENF   ASSIGN    TO        DA-01-VI-JHMKENL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       KEN-F01
                                           KEN-F02
                                           KEN-F03
                                           KEN-F04
                       FILE      STATUS    KEN-ST.
*受信件数マスタワーク
     SELECT  JHMKENWK  ASSIGN    TO        DA-01-S-JHMKENWK
                       FILE      STATUS    KEW-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 受信件数マスタ                                     *
****************************************************************
 FD  JHMKENF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JHMKENF   OF   XFDLIB
                       JOINING   KEN       AS   PREFIX.
*
****************************************************************
*    FILE = 受信件数マスタワーク                               *
****************************************************************
 FD  JHMKENWK
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JHMKENF   OF   XFDLIB
                       JOINING   KEW       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  KEN-ST                   PIC  X(02).
     03  KEW-ST                   PIC  X(02).
*時間編集領域
 01  WK-TIME.
     03  WK-TIME-HHMM             PIC  9(04)  VALUE  ZERO.
     03  WK-TIME-SS               PIC  9(04)  VALUE  ZERO.
*フラグ領域
 01  WK-FLG.
     03  END-FLG                  PIC  X(03)  VALUE  SPACE.
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  KEN-ERR           PIC N(15) VALUE
         NC"受信件数マスタエラー".
     03  KEW-ERR           PIC N(15) VALUE
         NC"受信件数マスタワークエラー".
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
*日付変換サブルーチン用ワーク
 01  LINK-IN-JDATE         PIC 9(08).
 01  LINK-IN-JTIME         PIC 9(04).
 01  LINK-IN-JTOKCD        PIC 9(08).
*
**************************************************************
 PROCEDURE             DIVISION   USING    LINK-IN-JDATE
                                           LINK-IN-JTIME
                                           LINK-IN-JTOKCD.
**************************************************************
 DECLARATIVES.
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
 KEW-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JHMKENWK.
     MOVE        KEW-ST    TO        E-ST.
     MOVE        "JHMKENWK" TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     KEW-ERR   UPON      CONS.
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
     OPEN      INPUT   JHMKENF.
     OPEN      OUTPUT  JHMKENWK.
*現在、時刻の取得
     ACCEPT    WK-TIME   FROM  TIME.
*受信件数マスタスタート
     MOVE      LINK-IN-JDATE  TO   KEN-F01.
     MOVE      LINK-IN-JTIME  TO   KEN-F02.
     MOVE      LINK-IN-JTOKCD TO   KEN-F03.
     MOVE      SPACE          TO   KEN-F04.
     START JHMKENF KEY IS >= KEN-F01 KEN-F02 KEN-F03 KEN-F04
               INVALID
               MOVE    "END"  TO   END-FLG
               GO             TO   INIT-EXIT
     END-START.
*
     PERFORM JHMKENF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*ファイル出力
     MOVE SPACE              TO  KEW-REC.
     INITIALIZE                  KEW-REC.
     MOVE KEN-REC            TO  KEW-REC.
     WRITE KEW-REC.
*
     PERFORM JHMKENF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 JHMKENF-READ-SEC      SECTION.
     MOVE     "JHMKENF-READ-SEC" TO   S-NAME.
*ファイル読込み
     READ      JHMKENF  NEXT  AT  END
               MOVE  "END"     TO   END-FLG
               GO              TO   JHMKENF-READ-EXIT
     END-READ.
*
     IF        LINK-IN-JDATE   =  KEN-F01
     AND       LINK-IN-JTIME   =  KEN-F02
     AND       LINK-IN-JTOKCD  =  KEN-F03
               CONTINUE
     ELSE
               MOVE    "END"   TO   END-FLG
     END-IF.
*
 JHMKENF-READ-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"      TO   S-NAME.
*ファイルのＯＰＥＮ
     CLOSE     JHMKENF  JHMKENWK.
*
 END-EXIT.
     EXIT.
*****************<<  SEG0040B   END PROGRAM  >>******************

```

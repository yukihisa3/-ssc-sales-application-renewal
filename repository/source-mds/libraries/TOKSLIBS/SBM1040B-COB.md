# SBM1040B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBM1040B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　イオン流通ＢＭＳ　　　　　　　　　*
*    業務名　　　　　　　：　イオン流通ＢＭＳ                  *
*    モジュール名　　　　：　発注データ変換処理起動（イオン）  *
*    作成日／更新日　　　：　12/11/22                          *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　イオン受信取引先ワークを読み、社内*
*                            イオン分の売上フォーマット変換処理*
*                            を起動する。                      *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SBM1040B.
 AUTHOR.               NAV.
 DATE-WRITTEN.         12/11/22.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     YA           IS   YA
     YB           IS   YB
     YA-22        IS   YA-22
     YB-21        IS   YB-21
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*イオン取引先受信ワーク
     SELECT  IONTWKF      ASSIGN    TO     DA-01-VI-IONTWKL1
                          ORGANIZATION     INDEXED
                          ACCESS    MODE   SEQUENTIAL
                          RECORD    KEY    TWK-F01
                                           TWK-F02
                                           TWK-F03
                                           TWK-F04
                          FILE      STATUS TWK-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = イオン取引先受信ワーク                             *
****************************************************************
 FD  IONTWKF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      IONTWKF   OF   XFDLIB
                       JOINING   TWK       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  TWK-ST                   PIC  X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  TWK-ERR           PIC N(15) VALUE
         NC"イオン取引先受信ワークエラー".
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
***  ﾜｰｸｴﾘｱ
 01  WK-AREA.
     03  END-FLG           PIC X(03) VALUE     SPACE.
     03  P-CNT             PIC 9(05) VALUE     ZERO.
     03  L-CNT             PIC 9(05) VALUE     ZERO.
     03  SYSDATE           PIC 9(06) VALUE     ZERO.
     03  IX                PIC 9(02) VALUE     ZERO.
***  日付取得
 01  WK-DATE8.
     03  WK-DATE8-YY1        PIC  9(02).
     03  WK-DATE8-YY2        PIC  9(06).
 01  WK-DATE8-R         REDEFINES  WK-DATE8.
     03  WK-YYYY             PIC  9(04).
     03  WK-MM               PIC  9(02).
     03  WK-DD               PIC  9(02).
 01  SYS-TIME.
     03  SYS-HH              PIC  9(02).
     03  SYS-MN              PIC  9(02).
     03  SYS-SS              PIC  9(02).
     03  FILLER              PIC  9(02).
 01  SYS-TIME2               PIC  9(08).
 01  FILLER            REDEFINES  SYS-TIME2.
     03  SYS-TIMEW           PIC  9(06).
     03  FILLER              PIC  9(02).
 01  WK-SYS-TIME             PIC  9(08)  VALUE  ZERO.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
*------------------------------------------------------------*
 LINKAGE              SECTION.
*------------------------------------------------------------*
*パラメタ取得
 01  LINK-KBN              PIC X(01).
 01  LINK-LINE             PIC X(01).
 01  LINK-YUSEN            PIC X(01).
*
**************************************************************
 PROCEDURE             DIVISION   USING    LINK-KBN
                                           LINK-LINE
                                           LINK-YUSEN.
**************************************************************
 DECLARATIVES.
 TWK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE IONTWKF.
     MOVE        TWK-ST    TO        E-ST.
     MOVE       "IONTWKL1" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TWK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     DISPLAY NC"＃発注データ変換処理起動　異常＃" UPON CONS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     DISPLAY NC"＃発注データ変換処理起動　開始＃" UPON CONS.
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC  UNTIL  END-FLG = "END".
     PERFORM   END-SEC.
     DISPLAY NC"＃発注データ変換処理起動　終了＃" UPON CONS.
     STOP  RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               0.0       *
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"     TO   S-NAME.
*ファイルのＯＰＥＮ
     OPEN      INPUT  IONTWKF.
*イオン取引先受信ワークスタート
     MOVE     SPACE       TO      TWK-REC.
     INITIALIZE                   TWK-REC.
     MOVE     LINK-KBN    TO      TWK-F01.
     START  IONTWKF  KEY  IS >=   TWK-F01  TWK-F02  TWK-F03
                                  TWK-F04
            INVALID
            DISPLAY NC"＃＃選択ＭＳＧ種ＤＴ無＃＃" UPON CONS
            MOVE  "END"   TO      END-FLG
            GO            TO      INIT-EXIT
     END-START.
*
     PERFORM  IONTWKF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*    変換ＰＧ起動
     CALL     "PBM00300"     USING   TWK-F02
                                     TWK-F03
                                     TWK-F04
                                     LINK-LINE
                                     LINK-YUSEN.
*次レコード読み込み
     PERFORM  IONTWKF-READ-SEC.
*終了していない場合で、取ＣＤ＝３３１８９の場合、１８０秒待合せ
     IF  END-FLG  NOT = "END"
         IF  TWK-F04 = 33189
             DISPLAY NC"＃変換待合中　開始" UPON CONS
             CALL "TWAIT180"
             DISPLAY NC"＃変換待合中　終了" UPON CONS
         END-IF
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*    イオン取引先受信ワーク読込                      ALL       *
****************************************************************
 IONTWKF-READ-SEC      SECTION.
     MOVE "IONTWKF-READ-SEC" TO   S-NAME.
*
     READ  IONTWKF  AT  END
           MOVE   "END"      TO   END-FLG
           GO                TO   IONTWKF-READ-EXIT
     END-READ.
*取引先ＣＤ＝９９９９９の場合は対象しない。
     IF    TWK-F04  =  99999
           GO                TO   IONTWKF-READ-SEC
     END-IF.
*パラメタデータ区分チェック
     IF    TWK-F01  NOT =  LINK-KBN
           MOVE   "END"      TO   END-FLG
     END-IF.
*
 IONTWKF-READ-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"      TO   S-NAME.
*ファイルのＯＰＥＮ
     CLOSE     IONTWKF.
*
 END-EXIT.
     EXIT.
*****************<<  SBM1040B   END PROGRAM  >>******************

```

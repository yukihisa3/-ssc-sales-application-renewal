# SBM1030L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBM1030L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　イオン流通ＢＭＳ　　　　　　　　　*
*    業務名　　　　　　　：　イオン流通ＢＭＳ                  *
*    モジュール名　　　　：　イオン取引先別受信件数リスト      *
*    作成日／更新日　　　：　12/11/22                          *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　アンサーファイルを読込み、終了ステ*
*                            ータスをパラメータの処理結果に設定*
*                            する。                            *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SBM1030L.
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
*取引先マスタ
     SELECT  HTOKMS       ASSIGN    TO     DA-01-VI-TOKMS2
                          ORGANIZATION     INDEXED
                          ACCESS    MODE   RANDOM
                          RECORD    KEY    TOK-F01
                          FILE      STATUS TOK-ST.
*プリント定義ファイル
     SELECT  PRTFILE      ASSIGN    TO     LP-04-PRTF
                          FILE      STATUS PRT-ST.
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
****************************************************************
*  FILE= 取引先マスタ                                          *
****************************************************************
 FD  HTOKMS            BLOCK     CONTAINS  8    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      HTOKMS    OF   XFDLIB
                       JOINING   TOK       AS   PREFIX.
****************************************************************
*  FILE=プリントファイル                                       *
****************************************************************
 FD  PRTFILE
                       LABEL     RECORD    IS   OMITTED.
 01  PRT-REC.
     03  FILLER            PIC X(200).
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  TWK-ST                   PIC  X(02).
     03  TOK-ST                   PIC  X(02).
     03  PRT-ST                   PIC  X(02).
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  TWK-ERR           PIC N(15) VALUE
         NC"イオン取引先受信ワークエラー".
     03  TOK-ERR           PIC N(15) VALUE
         NC"取引先マスタエラー".
     03  PRT-ERR           PIC N(15) VALUE
         NC"プリントファイルエラー".
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
     03  HTOKMS-INV-FLG    PIC X(03) VALUE     SPACE.
     03  LINK-ERRKBN       PIC X(01) VALUE     SPACE.
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
****************************************************************
*    プリントエリア                                            *
****************************************************************
*--------------------------------------------------------------*
*    ヘッダ                                                    *
*--------------------------------------------------------------*
*ヘッダ１
 01  HD1.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD1-01                  PIC  X(08).
     03  FILLER                  PIC  X(23)  VALUE  SPACE.
     03  FILLER                  PIC  N(16)  VALUE
         NC"＜イオン取引先別受信件数リスト＞"
                                 CHARACTER  TYPE  IS  YA-22.
     03  FILLER                  PIC  X(18)  VALUE  SPACE.
     03  HD1-02                  PIC  9(04).
     03  FILLER                  PIC  N(01)  VALUE  NC"年"
                                 CHARACTER  TYPE  IS  YA.
     03  HD1-03                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"月"
                                 CHARACTER  TYPE  IS  YA.
     03  HD1-04                  PIC  Z9.
     03  FILLER                  PIC  N(01)  VALUE  NC"日"
                                 CHARACTER  TYPE  IS  YA.
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  HD1-05                  PIC  ZZ9.
     03  FILLER                  PIC  N(01)  VALUE  NC"頁"
                                 CHARACTER  TYPE  IS  YA.
*ヘッダ２
 01  HD2.
     03  FILLER                  PIC  X(116) VALUE  SPACE.
     03  HD2-01                  PIC  99.
     03  FILLER                  PIC  N(01)  VALUE
                                 NC"："
                                 CHARACTER   TYPE   IS  YA.
     03  HD2-02                  PIC  99.
     03  FILLER                  PIC  N(01)  VALUE
                                 NC"："
                                 CHARACTER   TYPE   IS  YA.
     03  HD2-03                  PIC  99.
*ヘッダ３
 01  HD3.
     03  FILLER                  PIC  X(43)  VALUE  SPACE.
     03  FILLER                  PIC  N(08)  VALUE
                                 NC"受信バッチＮＯ："
                                 CHARACTER   TYPE   IS  YB-21.
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD3-01                  PIC  X(10).
     03  FILLER                  PIC  X(03)  VALUE  " - ".
     03  HD3-02                  PIC  X(05).
     03  FILLER                  PIC  X(01)  VALUE  SPACE.
     03  HD3-03                  PIC  9(08).
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  HD3-04                  PIC  N(15)
                                 CHARACTER   TYPE   IS  YA.
*ヘッダ４
 01  HD4.
     03  FILLER                  PIC  X(52)  VALUE  SPACE.
     03  FILLER                  PIC  N(05)  VALUE
                                 NC"ＭＳＧ種："
                                 CHARACTER   TYPE   IS  YB-21.
     03  HD4-01                  PIC  N(10)
                                 CHARACTER   TYPE   IS  YA.
*ヘッダ５
 01  HD5                         CHARACTER   TYPE   IS  YB-21.
     03  FILLER                  PIC  X(34)  VALUE  SPACE.
     03  FILLER                  PIC  N(05)  VALUE
                                 NC"受信取引先".
     03  FILLER                  PIC  X(30)  VALUE  SPACE.
     03  FILLER                  PIC  N(04)  VALUE
                                 NC"受信件数".
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  HD5-01                  PIC  N(04).
*線＝
 01  SEN1.
     03  FILLER                  PIC  X(136) VALUE  ALL "=".
*線－
 01  SEN2.
     03  FILLER                  PIC  X(136) VALUE  ALL "-".
*明細
 01  DT1                         CHARACTER  TYPE  IS  YA.
     03  FILLER                  PIC  X(35)  VALUE  SPACE.
     03  DT1-01                  PIC  9(08).
     03  FILLER                  PIC  X(02)  VALUE  SPACE.
     03  DT1-02                  PIC  N(15).
     03  FILLER                  PIC  X(06)  VALUE  SPACE.
     03  DT1-03                  PIC  ZZ,ZZZ,ZZ9.
     03  FILLER                  PIC  X(03)  VALUE  SPACE.
     03  DT1-04                  PIC  N(20).
*------------------------------------------------------------*
 LINKAGE              SECTION.
*------------------------------------------------------------*
*パラメタ取得
 01  LINK-KBN              PIC X(01).
 01  LINK-JDATE            PIC 9(08).
 01  LINK-JTIME            PIC 9(04).
*
**************************************************************
 PROCEDURE             DIVISION   USING    LINK-KBN
                                           LINK-JDATE
                                           LINK-JTIME.
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
     DISPLAY NC"＃取引先別受信件数リスト　異常＃" UPON CONS.
     STOP        RUN.
 TOK-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTOKMS.
     MOVE        TOK-ST    TO        E-ST.
     MOVE       "TOKMS2  " TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TOK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     DISPLAY NC"＃取引先別受信件数リスト　異常＃" UPON CONS.
     STOP        RUN.
 PRT-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE PRTFILE.
     MOVE        PRT-ST    TO        E-ST.
     MOVE        "PRTFILE " TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     PRT-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     DISPLAY NC"＃取引先別受信件数リスト　異常＃" UPON CONS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     DISPLAY NC"＃取引先別受信件数リスト　開始＃" UPON CONS.
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC  UNTIL  END-FLG = "END".
     PERFORM   END-SEC.
     DISPLAY NC"＃取引先別受信件数リスト　終了＃" UPON CONS.
     STOP  RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               0.0       *
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"     TO   S-NAME.
*ファイルのＯＰＥＮ
     OPEN      INPUT  IONTWKF  HTOKMS.
     OPEN      OUTPUT PRTFILE.
*
*システム日付・時刻の取得
     ACCEPT   SYSDATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYSDATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD        TO   WK-DATE8.
*    起動時の時刻で更新する。
     ACCEPT   SYS-TIME    FROM  TIME.
     MOVE     SYS-TIME            TO   WK-SYS-TIME.
*行カウンターを初期セット
     MOVE     99          TO      L-CNT.
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
*行カウンターチェック
     IF    L-CNT  >  61
           PERFORM  HEAD-WT-SEC
     END-IF.
*明細セット
*    受信取引先ＣＤ
     MOVE  TWK-F04           TO   DT1-01.
*    受信取引先名取得
     MOVE  TWK-F04           TO   TOK-F01.
     PERFORM  HTOKMS-READ-SEC.
*
     IF  HTOKMS-INV-FLG = "INV"
         MOVE  ALL NC"＊"    TO   DT1-02
     ELSE
         MOVE  TOK-F02       TO   DT1-02
     END-IF.
*    受信件数
     MOVE  TWK-F05           TO   DT1-03.
*    発注メッセージ判定
     IF  LINK-KBN NOT = "1"
         MOVE    SPACE     TO   HD5-01 DT1-04
         GO                TO   MAIN010
     END-IF.
*****変換ＯＫの場合
     MOVE     NC"変換結果" TO   HD5-01.
     IF  TWK-F06 = "0"
         MOVE NC"変換ＯＫ！！受信完了までお待ち下さい！！"
                           TO   DT1-04
         GO                TO   MAIN010
     END-IF.
*****変換ＮＧの場合
     IF  TWK-F06 = "1"
         MOVE NC"変換ＮＧ！！原因を調査しＮＡＶへ連絡！！"
                           TO   DT1-04
     END-IF.
 MAIN010.
*明細部出力
     WRITE    PRT-REC   FROM      DT1       AFTER  1.
     WRITE    PRT-REC   FROM      SEN2      AFTER  2.
     MOVE     SPACE               TO        PRT-REC.
     WRITE    PRT-REC   AFTER  1.
*    行カウンター
     ADD      4                       TO   L-CNT.
*次レコード読み込み
     PERFORM  IONTWKF-READ-SEC.
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
*パラメタデータ区分チェック
     IF    TWK-F01  NOT =  LINK-KBN
           MOVE   "END"      TO   END-FLG
     END-IF.
*
 IONTWKF-READ-EXIT.
     EXIT.
****************************************************************
*    取引先マスタ読込                                ALL       *
****************************************************************
 HTOKMS-READ-SEC       SECTION.
     MOVE "HTOKMS-READ-SEC"  TO   S-NAME.
*
     READ  HTOKMS
           INVALID      MOVE  "INV"   TO   HTOKMS-INV-FLG
           NOT  INVALID MOVE  SPACE   TO   HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*    ヘッダ印字処理                                  ALL       *
****************************************************************
 HEAD-WT-SEC           SECTION.
     MOVE "HEAD-WT-SEC"      TO   S-NAME.
*改頁チェック
     IF  P-CNT  >  ZERO
         MOVE   SPACE        TO   PRT-REC
         WRITE  PRT-REC      AFTER   PAGE
     END-IF.
*ヘッダを出力する。
*    ＰＧＩＤ
     MOVE      "SBM1030L"             TO   HD1-01.
*    システム日付
     MOVE      WK-YYYY                TO   HD1-02.
     MOVE      WK-MM                  TO   HD1-03.
     MOVE      WK-DD                  TO   HD1-04.
*    頁
     ADD       1                      TO   P-CNT.
     MOVE      P-CNT                  TO   HD1-05.
*    時刻
     MOVE      SYS-HH                 TO   HD2-01.
     MOVE      SYS-MN                 TO   HD2-02.
     MOVE      SYS-SS                 TO   HD2-03.
*    受信バッチ番号（日付）
     MOVE      LINK-JDATE(1:4)        TO   HD3-01(1:4).
     MOVE      LINK-JDATE(5:2)        TO   HD3-01(6:2).
     MOVE      LINK-JDATE(7:2)        TO   HD3-01(9:2).
     MOVE      "/"                    TO   HD3-01(5:1).
     MOVE      "/"                    TO   HD3-01(8:1).
*    受信バッチ番号（時刻）
     MOVE      LINK-JTIME(1:2)        TO   HD3-02(1:2).
     MOVE      LINK-JTIME(3:2)        TO   HD3-02(4:2).
     MOVE      ":"                    TO   HD3-02(3:1).
*    データ区分判定
     EVALUATE  LINK-KBN
         WHEN  "1"
         MOVE NC"発注メッセージ"      TO   HD4-01
         WHEN  "2"
         MOVE NC"受領メッセージ"      TO   HD4-01
         WHEN  "3"
         MOVE NC"受領訂正メッセージ"  TO   HD4-01
         WHEN  "4"
         MOVE NC"返品メッセージ"      TO   HD4-01
         WHEN  "5"
         MOVE NC"支払メッセージ"      TO   HD4-01
         WHEN  OTHER
         MOVE ALL NC"＊"              TO   HD4-01
     END-EVALUATE.
 MAIN040.
*ヘッダ部出力
     WRITE    PRT-REC   FROM      HD1       AFTER  3.
     WRITE    PRT-REC   FROM      HD2       AFTER  1.
     WRITE    PRT-REC   FROM      HD3       AFTER  1.
     WRITE    PRT-REC   FROM      HD4       AFTER  2.
     WRITE    PRT-REC   FROM      SEN1      AFTER  3.
     WRITE    PRT-REC   FROM      HD5       AFTER  1.
     WRITE    PRT-REC   FROM      SEN1      AFTER  1.
     MOVE     SPACE               TO        PRT-REC.
     WRITE    PRT-REC   AFTER  1.
*行カウンター
     MOVE     14                  TO        L-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"      TO   S-NAME.
*ファイルのＯＰＥＮ
     CLOSE     IONTWKF.
     CLOSE     HTOKMS.
     CLOSE     PRTFILE.
     DISPLAY NC"＃出力頁　＝　" P-CNT  "　＃"  UPON CONS.
*
 END-EXIT.
     EXIT.
*****************<<  SBM1030L   END PROGRAM  >>******************

```

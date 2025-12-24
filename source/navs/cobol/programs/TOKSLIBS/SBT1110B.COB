****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＨＧ基幹システム　　　　　　　　　*
*    業務名　　　　　　　：　甘木ＬＩＮＫＳ連携　　　　　　　　*
*    モジュール名　　　　：　出荷連携データ抽出（ナフコ手書）  *
*    作成日／更新日　　　：　2015/07/22                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受け取った各パラメタより、連携    *
*                            対象データをナフコ基本情報ファイル*
*                            より抽出する。                    *
*　　更新日／更新者　　　：　2015/09/30                        *
*　　更新日／更新者　　　：　NAV TAKAHASHI                     *
*    変更概要　　　　　　：　ナフコ　琴海店／小野田店対応　　　*
*      　　　　　　　　　　　                                  *
*　　更新日／更新者　　　：　                                  *
*　　更新日／更新者　　　：　2022/03/14 NAV TAKAHASHI          *
*    変更概要　　　　　　：　２０分類変更に伴い改修            *
*      　　　　　　　　　　　                                  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SBT1110B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2015/07/22.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*ナフコ基本情報
     SELECT   NFJOHOL1  ASSIGN    TO        DA-01-VI-NFJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       NFJ-F01   NFJ-F05
                                            NFJ-F06   NFJ-F07
                                            NFJ-F08   NFJ-F09
                        FILE  STATUS   IS   NFJ-ST.
*出荷連携データ
     SELECT   LNKSYKF   ASSIGN    TO        DA-01-S-LNKSYKF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SYU-ST.
*商品名称マスタ
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011
                                            MEI-F0121
                                            MEI-F0122
                                            MEI-F0123
                        FILE      STATUS    IS   MEI-ST.
*商品変換ＴＢＬ
     SELECT  HSHOTBL     ASSIGN    TO       DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SHO-F01 SHO-F02
                        FILE      STATUS    IS   SHO-ST.
*店舗マスタ
     SELECT  HTENMS      ASSIGN    TO       DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52 TEN-F011
                        FILE      STATUS    IS   TEN-ST.
*出荷パターンマスタ
     SELECT  LNKSPMF     ASSIGN    TO       DA-01-VI-LNKSPML1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SPM-F01 SPM-F02
                                            SPM-F03 SPM-F04
                        FILE      STATUS    IS   SPM-ST.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    ナフコ基本情報　ＲＬ＝１０２０
******************************************************************
 FD  NFJOHOL1
                        LABEL RECORD   IS   STANDARD.
     COPY     NFJOHOL1  OF        XFDLIB
*****COPY     JTCDENF   OF        XFDLIB
              JOINING   NFJ  AS   PREFIX.
*
******************************************************************
*    出荷連携データ
******************************************************************
 FD  LNKSYKF           BLOCK     CONTAINS   25  RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     LNKSYKF  OF        XFDLIB
              JOINING   SYU       PREFIX.
******************************************************************
*    商品名称マスタ
******************************************************************
 FD  HMEIMS             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
******************************************************************
*    商品変換ＴＢＬ
******************************************************************
 FD  HSHOTBL             LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL    OF       XFDLIB
              JOINING   SHO       PREFIX.
******************************************************************
*    店舗マスタ
******************************************************************
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
******************************************************************
*    出荷パターンマスタ
******************************************************************
 FD  LNKSPMF             LABEL RECORD   IS   STANDARD.
     COPY     LNKSPMF    OF       XFDLIB
              JOINING   SPM       PREFIX.
******************************************************************
 WORKING-STORAGE        SECTION.
*ワーク項目
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT1                PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT2                PIC  9(08)     VALUE  ZERO.
 01  WK-NFJ-F09              PIC  9(08)     VALUE  ZERO.
 01  HMEIMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  HSHOTBL-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  HTENMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  LNKSPMF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  HIDUKE-HENKAN           PIC  9(08)     VALUE  ZERO.
 01  WK-DEN-F15              PIC  9(10)     VALUE  ZERO.
 01  WK-DEN-F50              PIC  9(10)     VALUE  ZERO.
 01  AMARI                   PIC  9(02)     VALUE  ZERO.
 01  TANE-SIZAI              PIC  X(01)     VALUE  SPACE.
 01  WK-MEI-F07              PIC  9999.99.
 01  WK-SUURYOU              PIC  999999999.99.
*##2015/09/30 NAV ST ナフコ店舗対応
 01  WK-NFJ-F06              PIC  9(05)     VALUE  ZERO.
*##2015/09/30 NAV ED ナフコ店舗対応
*プログラムＳＴＡＴＵＳ.
 01  WK-ST.
     03  NFJ-ST        PIC  X(02).
     03  SYU-ST        PIC  X(02).
     03  MEI-ST        PIC  X(02).
     03  SHO-ST        PIC  X(02).
     03  TEN-ST        PIC  X(02).
     03  SPM-ST        PIC  X(02).
*バッチ_
 01  WK-BACHI-NO           PIC  X(20).
 01  WK-BACHI-NO-R         REDEFINES  WK-BACHI-NO.
     03  WK-BACHI-NO-1     PIC  9(08).
     03  WK-BACHI-NO-2     PIC  9(04).
     03  WK-BACHI-NO-3     PIC  9(08).
*部門コード退避
 01  WK-BUMON.
     03  WK-BUMON-1        PIC  9(04).
     03  WK-BUMON-2        PIC  9(08).
     03  WK-BUMON-3        PIC  9(04).
     03  WK-BUMON-4        PIC  X(04).
*店舗変換
 01  WK-TENPO-CD           PIC  9(06).
 01  WK-TENPO-CD-R         REDEFINES  WK-TENPO-CD.
     03  WK-TENPO-CD-H     PIC  X(06).
***** システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD              PIC   9(06)  VALUE  ZERO.
     03  SYS-DATEW           PIC   9(08)  VALUE  ZERO.
     03  SYS-DATE-R          REDEFINES SYS-DATEW.
         05  SYS-YY          PIC   9(04).
         05  SYS-MM          PIC   9(02).
         05  SYS-DD          PIC   9(02).
***** システム時刻ワーク
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HHMMSS     PIC  9(06).
     03  SYS-MS         PIC  9(02).

 01  MSG-AREA.
     03  MSG-WAKU           PIC  N(21)  VALUE
         NC"＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊".
     03  MSG-01             PIC  N(21)  VALUE
         NC"＊　以下の連携Ｎｏでデータを抽出します　＊".
     03  MSG-02.
         05  FILLER         PIC  X(04)  VALUE "＊".
         05  FILLER         PIC  X(12)  VALUE SPACE.
         05  MSG-02-RENNO   PIC  X(09).
         05  FILLER         PIC  X(17)  VALUE SPACE.
         05  FILLER         PIC  X(04)  VALUE "＊".

***  セクション名
 01  SEC-NAME.
     03  FILLER             PIC  X(05)     VALUE " *** ".
     03  S-NAME             PIC  X(30).
*メッセージ出力
 01  FILE-ERR.
     03  URI-ERR           PIC  N(20)  VALUE
         NC"ナフコ基本情報ファイルエラー".
     03  SYU-ERR           PIC  N(20)  VALUE
         NC"出荷連携データエラー".
     03  MEI-ERR           PIC  N(20)  VALUE
         NC"商品名称マスタエラー".
     03  SHO-ERR           PIC  N(20)  VALUE
         NC"商品変換ＴＢＬエラー".
     03  TEN-ERR           PIC  N(20)  VALUE
         NC"店舗マスタエラー".
     03  SPM-ERR           PIC  N(20)  VALUE
         NC"出荷パターンマスタエラー".
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SBT1110B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SBT1110B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(10)  VALUE " INPUT  = ".
         05  IN-CNT         PIC   9(08).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(10)  VALUE " OUTPUT = ".
         05  OUT-CNT        PIC   9(08).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT1.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(10)  VALUE " KANRI  = ".
         05  OUT-CNT1       PIC   9(08).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*    日付変換ワーク（パラメタ用）
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
***** 文字変換ワーク
 01  WK-HENKAN.
     03  WK-TENCDW           PIC   X(05)  VALUE  SPACE.
     03  WK-TENCDW-R         REDEFINES WK-TENCDW.
         05  WK-TENCDH       PIC   9(05).
*パラメタ定義
 LINKAGE                SECTION.
* 入力パラメタ
*    部門CD
 01  PARA-IN-BUMONCD        PIC   X(04).
*    担当者CD
 01  PARA-IN-TANCD          PIC   X(02).
*    送信区分
 01  PARA-IN-SOUSIN-KB      PIC   X(01).
*    抽出区分（オンライン）
 01  PARA-IN-CYUSYUTU-ONL   PIC   X(01).
*    抽出区分（手書き）
 01  PARA-IN-CYUSYUTU-TEG   PIC   X(01).
*    抽出区分（横持）
 01  PARA-IN-CYUSYUTU-YOK   PIC   X(01).
*    抽出倉庫CD
 01  PARA-IN-SOUKO          PIC   X(02).
*    指定（オンライン）-バッチ_（受信日）
 01  PARA-IN-ONL-JUSIN-HI   PIC   9(08).
*    指定（オンライン）-バッチ_（受信時刻）
 01  PARA-IN-ONL-JUSIN-JI   PIC   9(04).
*    指定（オンライン）-バッチ_（取引先）
 01  PARA-IN-ONL-JUSIN-TOR  PIC   9(08).
*    指定（オンライン）-納品日（FROM）
 01  PARA-IN-ONL-NOUHIN-FR  PIC   9(08).
*    指定（オンライン）-納品日（TO）
 01  PARA-IN-ONL-NOUHIN-TO  PIC   9(08).
*    指定（オンライン）-店舗（FROM）
 01  PARA-IN-ONL-TENPO-FR   PIC   9(05).
*    指定（オンライン）-店舗（TO）
 01  PARA-IN-ONL-TENPO-TO   PIC   9(05).
*    指定（手書き）-取引先CD
 01  PARA-IN-TEG-TOR        PIC   9(08).
*    指定（手書き）-管理番号（ナフコのみ）
 01  PARA-IN-TEG-KAN        PIC   9(08).
*    指定（手書き）-伝票NO（FROM）
 01  PARA-IN-TEG-DEN-FR     PIC   9(09).
*    指定（手書き）-伝票NO（TO）
 01  PARA-IN-TEG-DEN-TO     PIC   9(09).
*    指定（手書き）-納品日（FROM）
 01  PARA-IN-TEG-NOUHIN-FR  PIC   9(08).
*    指定（手書き）-納品日（TO）
 01  PARA-IN-TEG-NOUHIN-TO  PIC   9(08).
*    指定（横持ち）-横持日
 01  PARA-IN-YOK-HI         PIC   9(08).
* 出力パラメタ
*    抽出種類
 01  PARA-OUT-SYURUI        PIC   X(01).
*    抽出件数
 01  PARA-OUT-KENSUU        PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE DIVISION USING   PARA-IN-BUMONCD
                            PARA-IN-TANCD
                            PARA-IN-SOUSIN-KB
                            PARA-IN-CYUSYUTU-ONL
                            PARA-IN-CYUSYUTU-TEG
                            PARA-IN-CYUSYUTU-YOK
                            PARA-IN-SOUKO
                            PARA-IN-ONL-JUSIN-HI
                            PARA-IN-ONL-JUSIN-JI
                            PARA-IN-ONL-JUSIN-TOR
                            PARA-IN-ONL-NOUHIN-FR
                            PARA-IN-ONL-NOUHIN-TO
                            PARA-IN-ONL-TENPO-FR
                            PARA-IN-ONL-TENPO-TO
                            PARA-IN-TEG-TOR
                            PARA-IN-TEG-KAN
                            PARA-IN-TEG-DEN-FR
                            PARA-IN-TEG-DEN-TO
                            PARA-IN-TEG-NOUHIN-FR
                            PARA-IN-TEG-NOUHIN-TO
                            PARA-IN-YOK-HI
                            PARA-OUT-SYURUI
                            PARA-OUT-KENSUU.
 DECLARATIVES.
 URI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NFJOHOL1.
     DISPLAY     URI-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     NFJ-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SYU-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE LNKSYKF.
     DISPLAY     SYU-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SYU-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 MEI-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HMEIMS.
     DISPLAY     MEI-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     MEI-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SHO-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HSHOTBL.
     DISPLAY     SHO-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SHO-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TEN-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE HTENMS.
     DISPLAY     TEN-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     TEN-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SPM-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE LNKSPMF.
     DISPLAY     SPM-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SPM-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG    =   "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       NFJOHOL1.
     OPEN     INPUT     HMEIMS.
     OPEN     INPUT     HSHOTBL.
     OPEN     INPUT     HTENMS.
     OPEN     INPUT     LNKSPMF.
     OPEN     EXTEND    LNKSYKF.
     DISPLAY  MSG-START UPON CONS.
*システム時刻取得
     ACCEPT   SYS-TIME  FROM      TIME.
*システム日付取得
     ACCEPT   SYSYMD    FROM      DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYSYMD    TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
              MOVE      LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
              MOVE    ZERO             TO   SYS-DATEW
     END-IF.
*
     MOVE     SPACE     TO        END-FLG.
     MOVE     ZERO      TO        RD-CNT    WRT-CNT1.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*    パラメタ　バッチ_変換
*    MOVE     PARA-IN-BACHI-NO   TO   WK-BACHI-NO.
*    DISPLAY "#ﾊﾞｯﾁNO. = " WK-BACHI-NO-1 "-"
*                          WK-BACHI-NO-2 "-"
*                          WK-BACHI-NO-3 " #"  UPON CONS.
*基本情報ファイルスタート
     MOVE     SPACE          TO   NFJ-REC.
     INITIALIZE                   NFJ-REC.
     MOVE     PARA-IN-TEG-KAN     TO   NFJ-F01.
     MOVE     PARA-IN-SOUKO       TO   NFJ-F05.
     MOVE     ZERO                TO   NFJ-F06.
     MOVE     ZERO                TO   NFJ-F07.
     MOVE     ZERO                TO   NFJ-F08.
     MOVE     ZERO                TO   NFJ-F09.
     START    NFJOHOL1  KEY  >=   NFJ-F01   NFJ-F05
                                  NFJ-F06   NFJ-F07
                                  NFJ-F08   NFJ-F09
         INVALID   KEY
              MOVE "END"     TO   END-FLG
              GO             TO   INIT-EXIT
     END-START.
*基本情報ファイル読込
     PERFORM  NFJOHOL1-READ-SEC.
     IF       END-FLG = "END"
              GO             TO   INIT-EXIT
     END-IF.
*
 INIT-010.
*
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*レコード初期化
     MOVE     SPACE               TO   SYU-REC.
     INITIALIZE                        SYU-REC.
*出荷区分
     MOVE     "01"                TO   SYU-F01.
*納品日
     MOVE     NFJ-F09             TO   WK-NFJ-F09.
     MOVE     WK-NFJ-F09          TO   SYU-F02.
*伝票番号　　
     MOVE     NFJ-F07             TO   SYU-F03.
*取引先コード
     MOVE     NFJ-F04             TO   SYU-F04.
*店舗コード
     MOVE     NFJ-F06             TO   SYU-F05.
*受信日
     MOVE     SYS-DATEW           TO   SYU-F06.
*受信時刻
     MOVE     SYS-HHMMSS          TO   SYU-F07.
*店舗マスタ索引
     MOVE     NFJ-F04             TO   TEN-F52.
     MOVE     NFJ-F06             TO   TEN-F011.
     PERFORM HTENMS-READ-SEC.
     IF      HTENMS-INV-FLG  =  "INV"
             GO                   TO   MAIN-99
     END-IF.
*出荷パターンマスタ索引
     MOVE     PARA-IN-SOUKO       TO   SPM-F01.
     MOVE     NFJ-F04             TO   SPM-F02.
     MOVE     "2"                 TO   SPM-F03.
     MOVE     TEN-F12(1)          TO   SPM-F04.
     PERFORM LNKSPMF-READ-SEC.
     IF      LNKSPMF-INV-FLG  =  "INV"
             MOVE SPACE           TO   SPM-F05
     END-IF.
*出荷パターンコードにより店舗、届先、仕分１２、部門、計上セット
     EVALUATE   SPM-F05
         WHEN   "D1"
            MOVE   NFJ-F06        TO   SYU-F05
            MOVE   SPACE          TO   SYU-F08
            MOVE   NFJ-F06        TO   SYU-F09
            MOVE   SPACE          TO   SYU-F10
            MOVE   SPACE          TO   SYU-F11
            MOVE   SPACE          TO   SYU-F12
************2015/09/30 NAV ST ナフコ店舗対応
            IF  NFJ-F04 = 137607
                IF  NFJ-F06 = 81  OR  196
                    IF  NFJ-HE20 = "1"
                        COMPUTE WK-NFJ-F06 = 10000 + NFJ-F06
                        MOVE    WK-NFJ-F06   TO   SYU-F05
                        MOVE    WK-NFJ-F06   TO   SYU-F09
                    END-IF
                END-IF
            END-IF
************2015/09/30 NAV ED ナフコ店舗対応
         WHEN   "D2"
            MOVE   SPM-F06(1:5)   TO   WK-TENCDW
            MOVE   WK-TENCDH      TO   SYU-F05
            MOVE   SPACE          TO   SYU-F08
            MOVE   WK-TENCDH      TO   SYU-F09
            MOVE   NFJ-F06        TO   SYU-F10
            MOVE   SPACE          TO   SYU-F11
            MOVE   SPACE          TO   SYU-F12
         WHEN   "D3"
            MOVE   NFJ-F06        TO   SYU-F05
            MOVE   SPM-F06(1:5)   TO   WK-TENCDW
            MOVE   WK-TENCDH      TO   SYU-F08
            MOVE   NFJ-F06        TO   SYU-F09
            MOVE   SPACE          TO   SYU-F10
            MOVE   SPACE          TO   SYU-F11
            MOVE   SPACE          TO   SYU-F12
            MOVE   SPACE          TO   SYU-F08
*           IF  DEN-F131 = "99"
*                  MOVE  SPACE    TO   SYU-F08
*           END-IF
         WHEN   "D4"
            MOVE   NFJ-F06        TO   SYU-F05
            MOVE   SPACE          TO   SYU-F08
            MOVE   NFJ-F06        TO   SYU-F09
            MOVE   SPACE          TO   SYU-F10
*           MOVE   DEN-XI0004(22:4) TO SYU-F11
            MOVE   SPACE            TO SYU-F11
            MOVE   "0"            TO   SYU-F12(1:1)
*           MOVE   DEN-XI0004(248:1) TO  SYU-F12(2:1)
            MOVE   SPACE             TO  SYU-F12(2:1)
         WHEN   "D6"
            MOVE   NFJ-F06        TO   SYU-F05
            MOVE   SPACE          TO   SYU-F08
            MOVE   NFJ-F06        TO   SYU-F09
            MOVE   SPACE          TO   SYU-F10
*           MOVE   DEN-F12        TO   SYU-F11
            MOVE   SPACE          TO   SYU-F11
            MOVE   SPACE          TO   SYU-F12
         WHEN   "D7"
            MOVE   NFJ-F06        TO   SYU-F05
            MOVE   SPACE          TO   SYU-F08
            MOVE   NFJ-F06        TO   SYU-F09
            MOVE   SPACE          TO   SYU-F10
*           MOVE   DEN-F12        TO   SYU-F11
            MOVE   SPACE          TO   SYU-F11
            MOVE   SPACE          TO   SYU-F12
         WHEN   "DB"
            MOVE   NFJ-F06        TO   SYU-F05
            MOVE   SPACE          TO   SYU-F08
            MOVE   NFJ-F06        TO   SYU-F09
            MOVE   SPACE          TO   SYU-F10
*           MOVE   DEN-XI0004(22:4) TO SYU-F11
            MOVE   SPACE            TO SYU-F11
            MOVE   "99"           TO   SYU-F12
         WHEN   OTHER
            DISPLAY  NC"出荷パターン→変換エラー！！" UPON CONS
            DISPLAY  " !"   NC"取＝" NFJ-F04   UPON CONS
            DISPLAY  " !"   NC"伝＝" NFJ-F07   UPON CONS
            DISPLAY  " !"   NC"行＝" NFJ-F08   UPON CONS
            DISPLAY  " !"   NC"店＝" NFJ-F06   UPON CONS
            DISPLAY  " !"   NC"納＝" NFJ-F09   UPON CONS
            DISPLAY  " !"   NC"サ商＝" NFJ-F15 UPON CONS
            DISPLAY  " !"   NC"相商＝" NFJ-F13 UPON CONS
            DISPLAY  " !"   NC"出Ｐ＝" SPM-F05 UPON CONS
                 GO       TO   MAIN-99
     END-EVALUATE.
*明細行番号
     MOVE     "0"                 TO   SYU-F13(1:1).
     MOVE     NFJ-F08             TO   SYU-F13(2:2).
*品名コード
     IF       SHO-F10     =       "2"
              MOVE       NFJ-F14  TO   SYU-F14
     ELSE
              IF         MEI-F06  =    SPACE
                     DISPLAY  NC"ＪＡＮＣＤ取得エラー！！"
                                                    UPON CONS
                     DISPLAY  " !" NC"取＝" NFJ-F04 UPON CONS
                     DISPLAY  " !" NC"伝＝" NFJ-F07 UPON CONS
                     DISPLAY  " !" NC"行＝" NFJ-F08 UPON CONS
                     DISPLAY  " !" NC"店＝" NFJ-F06 UPON CONS
                     DISPLAY  " !" NC"納＝" NFJ-F09 UPON CONS
                     DISPLAY  " !" NC"サ商＝" NFJ-F15  UPON CONS
                     DISPLAY  " !" NC"相商＝" NFJ-F13  UPON CONS
                         GO       TO   MAIN-99
              ELSE
                         MOVE     MEI-F06   TO   SYU-F14
              END-IF
     END-IF.
*出荷指示数
     IF       MEI-F89   =   "1"
              DIVIDE  NFJ-F20  BY MEI-F07  GIVING  WK-DEN-F15
                                           REMAINDER   AMARI
              IF     AMARI    NOT = 0
                     MOVE    MEI-F07    TO  WK-MEI-F07
                     MOVE    NFJ-F20    TO  WK-SUURYOU
                    DISPLAY NC"数量が入数で割り切れません！"
                                                  UPON CONS
                    DISPLAY " !" NC"商Ｃ＝" MEI-F01    UPON CONS
                    DISPLAY " !" NC"入数＝" WK-MEI-F07 UPON CONS
                    DISPLAY " !" NC"数量＝" WK-SUURYOU UPON CONS
                    DISPLAY " !" NC"取Ｃ＝" NFJ-F04    UPON CONS
                    DISPLAY " !" NC"店Ｃ＝" NFJ-F06    UPON CONS
                    DISPLAY " !" NC"納日＝" NFJ-F09    UPON CONS
                    DISPLAY " !" NC"伝票＝" NFJ-F07    UPON CONS
                    DISPLAY " !" NC"行＝" NFJ-F08  UPON CONS
                      GO      TO             MAIN-99
              END-IF
     ELSE
              MOVE    NFJ-F20             TO  WK-DEN-F15
     END-IF.
     MOVE     WK-DEN-F15          TO   SYU-F15.
*実出荷数
*実出荷数には出荷指示数と同値をセット
     MOVE     SYU-F15             TO   SYU-F16.
*ラベル張り替え区分
     IF       SHO-F10     =       " "      OR   "2"
              MOVE       "0"               TO   SYU-F17
     ELSE
              MOVE     SHO-F10             TO   SYU-F17
     END-IF.
*張り替えＪＡＮコード
     IF       SHO-F10     =       "1"
              MOVE     SHO-F02             TO   SYU-F18
     ELSE
              MOVE     SPACE               TO   SYU-F18
     END-IF.
*種資材区分
     MOVE     TANE-SIZAI          TO   SYU-F19.
*出荷連携データ出力
     WRITE    SYU-REC.
*基本情報ファイルへ連携済FLG更新
     MOVE     "1"                 TO   NFJ-F40.
     MOVE     SYS-DATEW           TO   NFJ-F41.
     REWRITE  NFJ-REC.
*対象件数確認
     ADD      1                   TO   WRT-CNT1.
*基本情報ファイル読込
 MAIN-99.
     PERFORM  NFJOHOL1-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*ＰＡＲＡ（ＯＵＴ）セット
     IF  WRT-CNT1 > ZERO
         MOVE "C"                       TO PARA-OUT-SYURUI
         MOVE WRT-CNT1                  TO PARA-OUT-KENSUU
     END-IF.
*プログラム終了メッセージ表示
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT1  TO      OUT-CNT.
*    MOVE      WRT-CNT2  TO      OUT-CNT1.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
*    DISPLAY   MSG-OUT1  UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*ファイルクローズ
     CLOSE     NFJOHOL1  LNKSYKF  HMEIMS   HSHOTBL
               HTENMS    LNKSPMF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　基本情報ファイル読込
****************************************************************
 NFJOHOL1-READ-SEC          SECTION.
*
     READ     NFJOHOL1
              AT  END       MOVE  "END"   TO  END-FLG
                            GO TO NFJOHOL1-READ-EXIT
              NOT AT  END   ADD    1      TO  RD-CNT
     END-READ.
*件数表示
     IF       RD-CNT(6:3)  =  "000" OR "500"
              DISPLAY "# READ-CNT = " RD-CNT "(" WRT-CNT1 ") #"
                      UPON CONS
     END-IF.
*管理番号チェック（ＰＡＲＡ管理番号ブレイクで終了）
     IF       PARA-IN-TEG-KAN =   NFJ-F01
              CONTINUE
     ELSE
              MOVE      "END"     TO   END-FLG
              GO                  TO   NFJOHOL1-READ-EXIT
     END-IF.
*物流連携ＦＬＧチェック
*  新規：未連携が対象
*  再送：連携済が対象
     IF       PARA-IN-SOUSIN-KB       =   " "
        IF       NFJ-F40                 =   "1"
                 GO                  TO   NFJOHOL1-READ-SEC
        END-IF
     ELSE
        IF       NFJ-F40             NOT =   "1"
                 GO                  TO   NFJOHOL1-READ-SEC
        END-IF
     END-IF.
*商品変換ＴＢＬ取得
     PERFORM    HSHOTBL-READ-SEC.
*商品名称マスタ取得
     PERFORM    HMEIMS-READ-SEC.
     IF         HMEIMS-INV-FLG    NOT =  SPACE
*2013/03/12 NAV ST  商品変換ＴＢＬが未登録の場合　ＭＳＧ出力
                IF  HSHOTBL-INV-FLG   NOT =  SPACE
                    DISPLAY  NC"商品変換ＴＢＬ未登録！"
                       " !"  NC"取Ｃ＝" NFJ-F04
                        "!"  NC"商Ｃ＝" NFJ-F14 UPON CONS
                END-IF
*2013/03/12 NAV ED
                GO            TO         NFJOHOL1-READ-SEC
     END-IF.
*
 NFJOHOL1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　商品名称マスタ読込
****************************************************************
 HMEIMS-READ-SEC            SECTION.
*
     MOVE     NFJ-F15       TO         MEI-F011.
     MOVE     NFJ-F16       TO         MEI-F012.
     READ     HMEIMS
              INVALID       MOVE  "INV"   TO  HMEIMS-INV-FLG
                            DISPLAY  NC"商品名称マスタ未登録！"
                             " !" NC"商Ｃ＝" NFJ-F15 " " NFJ-F16
                              "!" NC"相Ｃ＝" NFJ-F13
                                                       UPON CONS
                            GO            TO  HMEIMS-READ-EXIT
              NOT INVALID   MOVE  SPACE   TO  HMEIMS-INV-FLG
     END-READ.
*
     MOVE     " "   TO  TANE-SIZAI.
     IF   HMEIMS-INV-FLG = SPACE
          MOVE   "2"   TO  TANE-SIZAI
*#2022/03/14 NAV ST
**********IF  MEI-F09 = "01" OR "02" OR "03" OR "04" OR
*                       "05" OR "06" OR "07" OR "08"
*             MOVE   "1"   TO  TANE-SIZAI
*             GO     TO    HMEIMS-READ-EXIT
*         END-IF
*         IF  MEI-F09 = "13" OR "14"
*             MOVE   "2"   TO  TANE-SIZAI
*             GO     TO    HMEIMS-READ-EXIT
**********END-IF
          IF  MEI-F09 = "01" OR "02" OR "03"
              MOVE   "1"   TO  TANE-SIZAI
              GO     TO    HMEIMS-READ-EXIT
          END-IF
          IF  MEI-F09 = "05"
              MOVE   "2"   TO  TANE-SIZAI
              GO     TO    HMEIMS-READ-EXIT
          END-IF
*#2022/03/14 NAV ED
     END-IF.
*
 HMEIMS-READ-EXIT.
     EXIT.
****************************************************************
*    商品変換ＴＢＬ検索                                        *
****************************************************************
 HSHOTBL-READ-SEC        SECTION.
*
     MOVE  NFJ-F04               TO  SHO-F01.
     MOVE  NFJ-F14               TO  SHO-F02.
     READ  HSHOTBL
       INVALID
         MOVE  "INV"             TO  HSHOTBL-INV-FLG
*2013/03/08 NAV ST 商品変換テーブルが未登録の場合、ラベル張替
*                  区分に空白をセットする。
         MOVE  SPACE             TO  SHO-F10
*2013/03/08 NAV ED
         GO                      TO  HSHOTBL-READ-EXIT
       NOT INVALID
         MOVE  SPACE             TO  HSHOTBL-INV-FLG
     END-READ.
*    IF  NFJ-F15 = SPACE
*        MOVE  SHO-F031          TO  NFJ-F15
*        MOVE  SHO-F032          TO  NFJ-F16
*    END-IF.
 HSHOTBL-READ-EXIT.
     EXIT.
****************************************************************
*    出荷パターンマスタ検索                                    *
****************************************************************
 LNKSPMF-READ-SEC        SECTION.
*
     READ  LNKSPMF
       INVALID
         DISPLAY  NC"出荷パターンマスタ未登録！" UPON CONS
         DISPLAY  " !" NC"倉Ｃ＝" SPM-F01 UPON CONS
         DISPLAY  " !" NC"取Ｃ＝" SPM-F02 UPON CONS
         DISPLAY  " !" NC"Ｏ／手区分＝" SPM-F03 UPON CONS
         DISPLAY  " !" NC"センター区分＝" SPM-F04  UPON CONS
         MOVE  "INV"             TO  LNKSPMF-INV-FLG
         GO                      TO  LNKSPMF-READ-EXIT
       NOT INVALID
         MOVE  SPACE             TO  LNKSPMF-INV-FLG
     END-READ.
*
 LNKSPMF-READ-EXIT.
     EXIT.
****************************************************************
*    店舗マスタ検索　　　　　                                  *
****************************************************************
 HTENMS-READ-SEC          SECTION.
*
     READ  HTENMS
       INVALID
         DISPLAY  NC"店舗マスタ未登録！"
             " !" NC"取Ｃ＝" NFJ-F04
              "!" NC"店舗Ｃ＝" NFJ-F06   UPON CONS
         MOVE  "INV"             TO  HTENMS-INV-FLG
         GO                      TO  HTENMS-READ-EXIT
       NOT INVALID
         MOVE  SPACE             TO  HTENMS-INV-FLG
     END-READ.
*
 HTENMS-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

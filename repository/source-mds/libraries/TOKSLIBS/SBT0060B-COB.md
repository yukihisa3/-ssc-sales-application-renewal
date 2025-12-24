# SBT0060B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBT0060B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＨＧ基幹システム　　　　　　　　　*
*    業務名　　　　　　　：　ＬＩＮＫＳ連携　　　　　　　　　　*
*    モジュール名　　　　：　出荷連携データ抽出（横持：作業）　*
*    作成日／更新日　　　：　2012/10/05                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受け取った各パラメタより、連携    *
*                            対象データを作業実績ファイル　　　*
*                            より抽出する。                    *
*　　更新日／更新者　　　：　2022/03/14                        *
*　　更新日／更新者　　　：　NAV TAKAHASHI                     *
*    修正概要　　　　　　：　２０分類変更に伴う改修            *
*      　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*      　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SBT0060B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          12/10/05.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*作業実績ファイル３（送信区分＝新規　用）
     SELECT   SGYFILL3  ASSIGN    TO        DA-01-VI-SGYFILL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       SG3-F93   SG3-F04
                                            SG3-F06
                        FILE  STATUS   IS   SG3-ST.
*作業実績ファイル４（送信区分＝再送　用）
     SELECT   SGYFILL4  ASSIGN    TO        DA-01-VI-SGYFILL4
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       SG4-F93   SG4-F04
                                            SG4-F06   SG4-F94
                        FILE  STATUS   IS   SG4-ST.
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
*    SELECT  HSHOTBL     ASSIGN    TO       DA-01-VI-SHOTBL8
*                       ORGANIZATION        INDEXED
*                       ACCESS    MODE      RANDOM
*                       RECORD    KEY       SHO-F031
*                                           SHO-F0321
*                                           SHO-F0322
*                                           SHO-F0323
*                       FILE      STATUS    IS   SHO-ST.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    作業実績ファイル３
******************************************************************
 FD  SGYFILL3
                        LABEL RECORD   IS   STANDARD.
     COPY     SGYFILL3   OF        XFDLIB
              JOINING   SG3  AS   PREFIX.
*
******************************************************************
*    作業実績ファイル４
******************************************************************
 FD  SGYFILL4
                        LABEL RECORD   IS   STANDARD.
     COPY     SGYFILL4   OF        XFDLIB
              JOINING   SG4  AS   PREFIX.
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
*FD  HSHOTBL             LABEL RECORD   IS   STANDARD.
*    COPY     HSHOTBL    OF       XFDLIB
*             JOINING   SHO       PREFIX.
******************************************************************
 WORKING-STORAGE        SECTION.
*ワーク項目
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT1                PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT2                PIC  9(08)     VALUE  ZERO.
 01  WK-DEN-F112             PIC  9(08)     VALUE  ZERO.
 01  HMEIMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  HSHOTBL-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  HIDUKE-HENKAN           PIC  9(08)     VALUE  ZERO.
 01  WK-DEN-F15              PIC  9(10)     VALUE  ZERO.
 01  WK-DEN-F50              PIC  9(10)     VALUE  ZERO.
 01  AMARI                   PIC  9(02)     VALUE  ZERO.
 01  TANE-SIZAI              PIC  X(01)     VALUE  SPACE.
 01  WK-MEI-F07              PIC  9999.99.
 01  WK-SUURYOU              PIC  9999999.99.
*プログラムＳＴＡＴＵＳ.
 01  WK-ST.
     03  SG3-ST        PIC  X(02).
     03  SG4-ST        PIC  X(02).
     03  SYU-ST        PIC  X(02).
     03  MEI-ST        PIC  X(02).
     03  SHO-ST        PIC  X(02).
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
     03  SG3-ERR           PIC  N(20)  VALUE
         NC"作業実績ファイル３エラー".
     03  SG4-ERR           PIC  N(20)  VALUE
         NC"作業実績ファイル４エラー".
     03  SYU-ERR           PIC  N(20)  VALUE
         NC"出荷連携データエラー".
     03  MEI-ERR           PIC  N(20)  VALUE
         NC"商品名称マスタエラー".
     03  SHO-ERR           PIC  N(20)  VALUE
         NC"商品変換ＴＢＬエラー".
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SBT0060B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SBT0060B".
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
                            PARA-IN-TEG-DEN-FR
                            PARA-IN-TEG-DEN-TO
                            PARA-IN-TEG-NOUHIN-FR
                            PARA-IN-TEG-NOUHIN-TO
                            PARA-IN-YOK-HI
                            PARA-OUT-SYURUI
                            PARA-OUT-KENSUU.
 DECLARATIVES.
 SG3-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SGYFILL3.
     DISPLAY     SG3-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SG3-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SG4-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE SGYFILL4.
     DISPLAY     SG4-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     SG4-ST    UPON      CONS.
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
*SHO-ERR                   SECTION.
*    USE         AFTER     EXCEPTION PROCEDURE HSHOTBL.
*    DISPLAY     SHO-ERR   UPON      CONS.
*    DISPLAY     SEC-NAME  UPON      CONS.
*    DISPLAY     SHO-ST    UPON      CONS.
*    MOVE        "4000"    TO        PROGRAM-STATUS.
*    STOP        RUN.
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
*
*送信区分＝" "（新規）→Ｌ３　"1"再送→Ｌ４
     IF       PARA-IN-SOUSIN-KB   =    "1"
              OPEN      I-O       SGYFILL4
     ELSE
              OPEN      I-O       SGYFILL3
     END-IF.
     OPEN     INPUT     HMEIMS.
*    OPEN     INPUT     HSHOTBL.
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
     INITIALIZE                   WK-BUMON.
*送信区分＝" "（新規）→Ｌ３　"1"再送→Ｌ４
     IF       PARA-IN-SOUSIN-KB  =  "1"
              GO                    TO   INIT-02
     ELSE
              GO                    TO   INIT-01
     END-IF.
*
 INIT-01.
*作業実績ファイル３スタート（送信区分＝" "（新規））
     MOVE     SPACE                 TO   SG3-REC.
     INITIALIZE                          SG3-REC.
     MOVE     SPACE                 TO   SG3-F93.
     MOVE     PARA-IN-SOUKO         TO   SG3-F04.
     MOVE     2                     TO   SG3-F06.
     START    SGYFILL3  KEY  >=   SG3-F93   SG3-F04   SG3-F06
         INVALID   KEY
              MOVE "END"     TO   END-FLG
              GO             TO   INIT-EXIT
     END-START.
*作業実績ファイル３読込
     PERFORM  SGYFILL3-READ-SEC.
     IF       END-FLG = "END"
              GO             TO   INIT-EXIT
     END-IF.
*
     GO       TO   INIT-EXIT.
*
 INIT-02.
*作業実績ファイル４スタート（送信区分＝"1"（再送））
     MOVE     SPACE                 TO   SG4-REC.
     INITIALIZE                          SG4-REC.
     MOVE     "1"                   TO   SG4-F93.
     MOVE     PARA-IN-SOUKO         TO   SG4-F04.
     MOVE     2                     TO   SG4-F06.
     MOVE     PARA-IN-YOK-HI        TO   SG4-F94.
     START    SGYFILL4  KEY  >=   SG4-F93   SG4-F04
                                  SG4-F06   SG4-F94
         INVALID   KEY
              MOVE "END"     TO   END-FLG
              GO             TO   INIT-EXIT
     END-START.
*作業実績ファイル４読込
     PERFORM  SGYFILL4-READ-SEC.
     IF       END-FLG = "END"
              GO             TO   INIT-EXIT
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
*送信区分＝" "（新規）→Ｌ３　"1"再送→Ｌ４
     IF       PARA-IN-SOUSIN-KB  =  "1"
              PERFORM   MAIN-L4-SEC
     ELSE
              PERFORM   MAIN-L3-SEC
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理（入出庫ファイルＬ３より抽出）　　　　*
****************************************************************
 MAIN-L3-SEC     SECTION.
*
*レコード初期化
     MOVE     SPACE               TO   SYU-REC.
     INITIALIZE                        SYU-REC.
*出荷区分
     MOVE     "03"                TO   SYU-F01.
*納品日
     MOVE     SG3-F05             TO   SYU-F02.
*伝票番号　　
     MOVE     SG3-F01             TO   SYU-F03.
*取引先コード
     MOVE     "00000000    "      TO   SYU-F04.
*店舗コード
     MOVE     SG3-F04             TO   SYU-F05.
*受信日
     MOVE     SYS-DATEW           TO   SYU-F06.
*受信時刻
     MOVE     SYS-HHMMSS          TO   SYU-F07.
*届先コード
     MOVE     SG3-F04             TO   SYU-F08.
*仕分店コード１
     MOVE     SG3-F04             TO   SYU-F09.
*仕分店コード２
     MOVE     SG3-F04             TO   SYU-F10.
*部門コード
     MOVE     SPACE               TO   SYU-F11.
*形状コード　
     MOVE     SPACE               TO   SYU-F12.
*明細行番号
     MOVE     "0"                 TO   SYU-F13(1:1).
     MOVE     SG3-F02             TO   SYU-F13(2:2).
*品名コード
     IF       MEI-F06   =         SPACE
              GO        TO        MAIN-L3-99
     END-IF.
     MOVE     MEI-F06             TO   SYU-F14.
*出荷指示数
*    IF       MEI-F89   =   "1"
*             COMPUTE   SG3-F11   =    SG3-F11  /  MEI-F07
*    ELSE
*             COMPUTE   SG3-F11   =    SG3-F11
*    END-IF.
*    MOVE     SG3-F11             TO   WK-DEN-F15.
*    MOVE     WK-DEN-F15          TO   SYU-F15.
     IF       MEI-F89   =   "1"
              DIVIDE  SG3-F11  BY MEI-F07  GIVING  WK-DEN-F15
                                           REMAINDER   AMARI
              IF     AMARI    NOT = 0
                     DISPLAY  NC"数量が入数で割り切れません！"
                                                      UPON CONS
                     DISPLAY  NC"商品ＣＤ＝" MEI-F01  UPON CONS
                     MOVE    MEI-F07    TO  WK-MEI-F07
                     DISPLAY NC"入り数　＝" WK-MEI-F07 UPON CONS
                     MOVE    SG3-F11    TO  WK-SUURYOU
                     DISPLAY NC"数量　　＝" WK-SUURYOU UPON CONS
                     DISPLAY  NC"場所　　＝" SG3-F04  UPON CONS
                     DISPLAY  NC"作業日　＝" SG3-F05  UPON CONS
                     DISPLAY  NC"作業番号＝" SG3-F01  UPON CONS
                     DISPLAY  NC"行　　　＝" SG3-F02  UPON CONS
***                   MOVE     4000       TO  PROGRAM-STATUS
***                   MOVE     "END"      TO  END-FLG
***                   GO       TO             MAIN-EXIT
                      GO       TO             MAIN-L3-99
              END-IF
     ELSE
              MOVE    SG3-F11             TO  WK-DEN-F15
     END-IF.
     MOVE     WK-DEN-F15          TO   SYU-F15.
*実出荷数（出荷指示数と同一）
     MOVE     SYU-F15             TO   SYU-F16.
*ラベル張り替え区分
     MOVE     "0"                 TO   SYU-F17.
*張り替えＪＡＮコード
     MOVE     SPACE               TO   SYU-F18.
*種資材区分
     MOVE     TANE-SIZAI          TO   SYU-F19.
*出荷連携データ出力
     WRITE    SYU-REC.
*作業実績ファイル３へ連携済FLG更新
     MOVE     "1"                 TO   SG3-F93.
     MOVE     SYS-DATEW           TO   SG3-F94.
     REWRITE  SG3-REC.
*対象件数確認
     ADD      1                   TO   WRT-CNT1.
*作業実績ファイル３読込
 MAIN-L3-99.
     PERFORM  SGYFILL3-READ-SEC.
*
 MAIN-L3-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理（入出庫ファイルＬ４より抽出）　　　　*
****************************************************************
 MAIN-L4-SEC     SECTION.
*
*レコード初期化
     MOVE     SPACE               TO   SYU-REC.
     INITIALIZE                        SYU-REC.
*出荷区分
     MOVE     "03"                TO   SYU-F01.
*納品日
     MOVE     SG4-F05             TO   SYU-F02.
*伝票番号　　
     MOVE     SG4-F01             TO   SYU-F03.
*取引先コード
     MOVE     "00000000    "      TO   SYU-F04.
*店舗コード
     MOVE     SG4-F04             TO   SYU-F05.
*受信日
     MOVE     SYS-DATEW           TO   SYU-F06.
*受信時刻
     MOVE     SYS-HHMMSS          TO   SYU-F07.
*届先コード
     MOVE     SG4-F04             TO   SYU-F08.
*仕分店コード１
     MOVE     SG4-F04             TO   SYU-F09.
*仕分店コード２
     MOVE     SG4-F04             TO   SYU-F10.
*部門コード
     MOVE     SPACE               TO   SYU-F11.
*形状コード　
     MOVE     SPACE               TO   SYU-F12.
*明細行番号
     MOVE     "0"                 TO   SYU-F13(1:1).
     MOVE     SG4-F02             TO   SYU-F13(2:2).
*品名コード
     IF       MEI-F06   =         SPACE
              GO        TO        MAIN-L4-99
     END-IF.
     MOVE     MEI-F06             TO   SYU-F14.
*出荷指示数
*    IF       MEI-F89   =   "1"
*             COMPUTE   SG4-F11   =    SG4-F11  /  MEI-F07
*    ELSE
*             COMPUTE   SG4-F11   =    SG4-F11
*    END-IF.
*    MOVE     SG4-F11             TO   WK-DEN-F15.
*    MOVE     WK-DEN-F15          TO   SYU-F15.
     IF       MEI-F89   =   "1"
              DIVIDE  SG4-F11  BY MEI-F07  GIVING  WK-DEN-F15
                                           REMAINDER   AMARI
              IF     AMARI    NOT = 0
                     DISPLAY  NC"数量が入数で割り切れません！"
                                                      UPON CONS
                     DISPLAY  NC"商品ＣＤ＝" MEI-F01  UPON CONS
                     MOVE    MEI-F07    TO  WK-MEI-F07
                     DISPLAY NC"入り数　＝" WK-MEI-F07 UPON CONS
                     MOVE    SG4-F11    TO  WK-SUURYOU
                     DISPLAY NC"数量　　＝" WK-SUURYOU UPON CONS
                     DISPLAY  NC"場所　　＝" SG4-F04  UPON CONS
                     DISPLAY  NC"作業日　＝" SG4-F05  UPON CONS
                     DISPLAY  NC"作業番号＝" SG4-F01  UPON CONS
                     DISPLAY  NC"行　　　＝" SG4-F02  UPON CONS
***                   MOVE     4000       TO  PROGRAM-STATUS
***                   MOVE     "END"      TO  END-FLG
***                   GO       TO             MAIN-EXIT
                      GO       TO             MAIN-L4-99
              END-IF
     ELSE
              MOVE    SG4-F11             TO  WK-DEN-F15
     END-IF.
     MOVE     WK-DEN-F15          TO   SYU-F15.
*実出荷数（出荷指示数と同一）
     MOVE     SYU-F15             TO   SYU-F16.
*ラベル張り替え区分
     MOVE     "0"                 TO   SYU-F17.
*張り替えＪＡＮコード
     MOVE     SPACE               TO   SYU-F18.
*種資材区分
     MOVE     TANE-SIZAI          TO   SYU-F19.
*出荷連携データ出力
     WRITE    SYU-REC.
*作業実績ファイル４へ連携済FLG更新
     MOVE     "1"                 TO   SG4-F93.
     MOVE     SYS-DATEW           TO   SG4-F94.
     REWRITE  SG4-REC.
*対象件数確認
     ADD      1                   TO   WRT-CNT1.
*作業実績ファイル４読込
 MAIN-L4-99.
     PERFORM  SGYFILL4-READ-SEC.
*
 MAIN-L4-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*ＰＡＲＡ（ＯＵＴ）セット
     IF  WRT-CNT1 > ZERO
         MOVE "E"                       TO PARA-OUT-SYURUI
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
     IF        PARA-IN-SOUSIN-KB    =    "1"
               CLOSE     SGYFILL4
     ELSE
               CLOSE     SGYFILL3
     END-IF.
     CLOSE     LNKSYKF  HMEIMS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　作業実績ファイル３読込（送信区分＝" "新規 用）
****************************************************************
 SGYFILL3-READ-SEC          SECTION.
*
     READ     SGYFILL3
              AT  END       MOVE  "END"   TO  END-FLG
                            GO TO SGYFILL3-READ-EXIT
              NOT AT  END   ADD    1      TO  RD-CNT
     END-READ.
*件数表示
     IF       RD-CNT(6:3)  =  "000" OR "500"
              DISPLAY "# READ-CNT = " RD-CNT "(" WRT-CNT1 ") #"
                      UPON CONS
     END-IF.
*物流連携ＦＬＧチェック（" "ブレイクで終了）
     IF       SG3-F93    NOT  =  " "
              MOVE      "END"     TO   END-FLG
              GO                  TO   SGYFILL3-READ-EXIT
     END-IF.
*作業場所（ＰＡＲＡ抽出倉庫ブレイクで終了）
     IF       SG3-F04    NOT  =   PARA-IN-SOUKO
              MOVE      "END"     TO   END-FLG
              GO                  TO   SGYFILL3-READ-EXIT
     END-IF.
*入/出区分チェック（2ブレイクで終了）
     IF       SG3-F06    NOT  =   2
              MOVE      "END"     TO   END-FLG
              GO                  TO   SGYFILL3-READ-EXIT
     END-IF.
*作業区分（特定区分以外は読み飛ばし）
     IF       SG3-F03    =   "F1" OR "G1" OR "G2" OR "G3" OR "G5"
************************ OR  "55"
              CONTINUE
     ELSE
              GO                  TO   SGYFILL3-READ-SEC
     END-IF.
*数量が０以下の明細は抽出対象としない。
*    IF  SG3-F11  >  ZERO
*        CONTINUE
*    ELSE
*        GO TO  SGYFILL3-READ-SEC
*    END-IF.
*商品名称マスタ取得
     MOVE       SG3-F08       TO         MEI-F011.
     MOVE       SG3-F09       TO         MEI-F012.
     PERFORM    HMEIMS-READ-SEC.
     IF         HMEIMS-INV-FLG    NOT =  SPACE
                GO            TO         SGYFILL3-READ-SEC
     END-IF.
*商品変換ＴＢＬ取得
*    MOVE       SG3-F08       TO         SHO-F031.
*    MOVE       SG3-F09       TO         SHO-F032.
*    PERFORM    HSHOTBL-READ-SEC.
*
 SGYFILL3-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　作業実績ファイル４読込（送信区分＝"1"再送 用）
****************************************************************
 SGYFILL4-READ-SEC          SECTION.
*
     READ     SGYFILL4
              AT  END       MOVE  "END"   TO  END-FLG
                            GO TO SGYFILL4-READ-EXIT
              NOT AT  END   ADD    1      TO  RD-CNT
     END-READ.
*件数表示
     IF       RD-CNT(6:3)  =  "000" OR "500"
              DISPLAY "# READ-CNT = " RD-CNT "(" WRT-CNT1 ") #"
                      UPON CONS
     END-IF.
*物流連携ＦＬＧチェック（"1"ブレイクで終了）
     IF       SG4-F93    NOT  =  "1"
              MOVE      "END"     TO   END-FLG
              GO                  TO   SGYFILL4-READ-EXIT
     END-IF.
*作業場所（ＰＡＲＡ抽出倉庫ブレイクで終了）
     IF       SG4-F04    NOT  =   PARA-IN-SOUKO
              MOVE      "END"     TO   END-FLG
              GO                  TO   SGYFILL4-READ-EXIT
     END-IF.
*入/出区分チェック（2ブレイクで終了）
     IF       SG4-F06    NOT  =   2
              MOVE      "END"     TO   END-FLG
              GO                  TO   SGYFILL4-READ-EXIT
     END-IF.
*物流連携日（ＰＡＲＡ横持日ブレイクで終了）
     IF       SG4-F94    NOT  =   PARA-IN-YOK-HI
              MOVE      "END"     TO   END-FLG
              GO                  TO   SGYFILL4-READ-EXIT
     END-IF.
*作業区分（特定区分以外は読み飛ばし）
     IF       SG4-F03    =   "F1" OR "G1" OR "G2" OR "G3" OR "G5"
              CONTINUE
     ELSE
              GO                  TO   SGYFILL4-READ-SEC
     END-IF.
*数量が０以下の明細は抽出対象としない。
*    IF  SG4-F11  >  ZERO
*        CONTINUE
*    ELSE
*        GO TO  SGYFILL4-READ-SEC
*    END-IF.
*商品名称マスタ取得
     MOVE       SG4-F08       TO         MEI-F011.
     MOVE       SG4-F09       TO         MEI-F012.
     PERFORM    HMEIMS-READ-SEC.
     IF         HMEIMS-INV-FLG    NOT =  SPACE
                GO            TO         SGYFILL4-READ-SEC
     END-IF.
*商品変換ＴＢＬ取得
*    MOVE       SG4-F08       TO         SHO-F031.
*    MOVE       SG4-F09       TO         SHO-F032.
*    PERFORM    HSHOTBL-READ-SEC.
*
 SGYFILL4-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　商品名称マスタ読込
****************************************************************
 HMEIMS-READ-SEC            SECTION.
*
     READ     HMEIMS
              INVALID       MOVE  "INV"   TO  HMEIMS-INV-FLG
*I--------------------------MOVE  ?????   TO  TANE-SIZAI 種資材
*I--------------------------MOVE  ?????   TO  MEI-F89  束区分
*I--------------------------MOVE  1       TO  MEI-F07  入数
              NOT INVALID   MOVE  SPACE   TO  HMEIMS-INV-FLG
     END-READ.
     IF       HMEIMS-INV-FLG NOT = SPACE
        IF    PARA-IN-SOUSIN-KB    =    "1"
              DISPLAY NC"商品名称マスタ未登録！"     UPON CONS
              DISPLAY NC"商品ＣＤ＝" SG4-F08 SG4-F09 UPON CONS
***           MOVE  4000    TO  PROGRAM-STATUS
***           MOVE  "END"   TO  END-FLG
              GO            TO  HMEIMS-READ-EXIT
        ELSE
              DISPLAY NC"商品名称マスタ未登録！"     UPON CONS
              DISPLAY NC"商品ＣＤ＝" SG3-F08 SG3-F09 UPON CONS
***           MOVE  4000    TO  PROGRAM-STATUS
***           MOVE  "END"   TO  END-FLG
              GO            TO  HMEIMS-READ-EXIT
        END-IF
     END-IF.
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
*HSHOTBL-READ-SEC        SECTION.
*
*    READ  HSHOTBL
*      INVALID
*        MOVE  "INV"             TO  HSHOTBL-INV-FLG
*      NOT INVALID
*        MOVE  SPACE             TO  HSHOTBL-INV-FLG
*    END-READ.
*HSHOTBL-READ-EXIT.
*    EXIT.
*-------------< PROGRAM END >------------------------------------*

```

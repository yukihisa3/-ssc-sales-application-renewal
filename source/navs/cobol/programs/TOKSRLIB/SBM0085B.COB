******************************************************************
*
* 　  顧客名             ：(株)サカタのタネ殿
*   　業務名　　　       ：ＨＧ基幹システム
*   　サブシステム名     ：ＬＩＮＫＳ連携
*   　モジュール名       ：出荷送信フォーマット変換（ムサシ）
*   　作成日／更新日     ：2018/06/12 高橋
*   　作成日／更新者     ：
*   　処理概要           ：発行指示を受け抽出した出荷メッセ
*                          ージデータを、ＢＭＳ標準フォーマ
*                          ットに変換する。（手書）
*
******************************************************************
 IDENTIFICATION         DIVISION.
******************************************************************
 PROGRAM-ID.            SBM0085B.
******************************************************************
 AUTHOR.                NAV.
 DATE-WRITTEN.          18/06/12.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*流通ＢＭＳ出荷メッセージ
     SELECT   WKSYKAR1  ASSIGN         DA-01-VI-WKSYKAR1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SYK-F011
                                       SYK-F012
                                       SYK-F013
                                       SYK-F02
                                       SYK-F349
                                       SYK-F309
                                       SYK-F302
                                       SYK-F402
                        FILE STATUS    IS   SYK-ST.
*出荷メッセージ送信ファイル
     SELECT   SYKSNDF   ASSIGN    TO   DA-01-S-SYKSNDF
                        ACCESS    MODE SEQUENTIAL
                        FILE STATUS    IS   SND-ST.
*
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*流通ＢＭＳ出荷メッセージ
******************************************************************
 FD  WKSYKAR1
                        LABEL     RECORD   IS   STANDARD.
     COPY     WKSYKAR1   OF        XFDLIB
              JOINING   SYK       PREFIX.
******************************************************************
*出荷メッセージ送信ファイル
******************************************************************
 FD  SYKSNDF
                        BLOCK     CONTAINS  3   RECORDS
                        LABEL     RECORD   IS   STANDARD.
     COPY     SYKSNDF   OF        XFDLIB
              JOINING   SND       PREFIX.
******************************************************************
 WORKING-STORAGE        SECTION.
*ワーク項目
 01  END-FLG                      PIC  X(03)     VALUE  SPACE.
 01  END-FLG2                     PIC  X(03)     VALUE  SPACE.
 01  WK-KAISIBI                   PIC  X(08)     VALUE  SPACE.
 01  RD-CNT1                      PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT1                     PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT2                     PIC  9(08)     VALUE  ZERO.
 01  WKEY.
     03  TORI-NO                  PIC  X(10)     VALUE  SPACE.
     03  LAST-CD                  PIC  X(13)     VALUE  SPACE.
     03  LAST-DATE                PIC  9(08)     VALUE  ZERO.
*ワーク退避レコード
     COPY     WKSYKAR1   OF        XFDLIB
              JOINING   WSYK      PREFIX.
*ワーク明細データ退避
     COPY     SYKSNDF   OF        XFDLIB
              JOINING   WSND      PREFIX.
*プログラムＳＴＡＴＵＳ
 01  WK-ST.
     03  SYK-ST                   PIC  X(02).
     03  SND-ST                   PIC  X(02).
 01  WK-REC                       PIC  X(1200)   VALUE  SPACE.
 01  WK-REC2                      PIC  X(1200)   VALUE  SPACE.
*バッチ
*****  システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD                   PIC  9(06)     VALUE  ZERO.
     03  SYS-DATEW                PIC  9(08)     VALUE  ZERO.
     03  SYS-DATE-R               REDEFINES SYS-DATEW.
         05  SYS-YY               PIC  9(04).
         05  SYS-MM               PIC  9(02).
         05  SYS-DD               PIC  9(02).
*****  システム時刻ワーク
 01  SYS-TIME                     PIC  9(08).
 01  FILLER                       REDEFINES      SYS-TIME.
     03  SYS-HHMMSS               PIC  9(06).
     03  SYS-MS                   PIC  9(02).
***  セクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(05)     VALUE " *** ".
     03  S-NAME                   PIC  X(30).
*メッセージ出力
 01  FILE-ERR.
     03  SYK-ERR                  PIC  N(20)     VALUE
         NC"流通ＢＭＳ出荷メッセージエラ－".
     03  SND-ERR                  PIC  N(20)     VALUE
         NC"出荷メッセージ送信ファイルエラ－".
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "SBM0085B".
         05  FILLER               PIC  X(11)     VALUE
                                        " START *** ".
     03  MSG-END.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "SBM0085B".
         05  FILLER               PIC  X(11)     VALUE
                                        " END   *** ".
*    日付変換ワーク（パラメタ用）
 01  LINK-AREA.
     03  LINK-IN-KBN              PIC  X(01).
     03  LINK-IN-YMD6             PIC  9(06).
     03  LINK-IN-YMD8             PIC  9(08).
     03  LINK-OUT-RET             PIC  X(01).
     03  LINK-OUT-YMD8            PIC  9(08).
*パラメタ定義
 LINKAGE                SECTION.
 01  PARA-KENSUU            PIC  9(07).
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE DIVISION         USING   PARA-KENSUU.
 DECLARATIVES.
 SYK-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  WKSYKAR1.
     DISPLAY       SYK-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       SYK-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 SND-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  SYKSNDF.
     DISPLAY       SND-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       SND-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 END       DECLARATIVES.
******************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"      TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =    "END".
     PERFORM  END-SEC.
*
******************************************************************
*             初期処理                                         *
******************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"           TO        S-NAME.
     OPEN     I-O       WKSYKAR1.
     OPEN     OUTPUT    SYKSNDF.
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
              MOVE      ZERO           TO   SYS-DATEW
     END-IF.
*
     MOVE     SPACE                TO        END-FLG.
     MOVE     ZERO                 TO        RD-CNT1
                                             WRT-CNT1
                                             WRT-CNT2.
*
*出荷メッセージファイル読込
     PERFORM  WKSYKAR1-READ-SEC.
*
 INIT-EXIT.
     EXIT.
******************************************************************
*              メイン処理                                      *
******************************************************************
 MAIN-SEC     SECTION.
*
     MOVE     "MAIN-SEC"           TO        S-NAME.
*各レコードの編集・更新
*出荷メッセージ送信ファイルの作成
**** IF       RD-CNT1    =         1
     IF       WRT-CNT1   =  0
              MOVE       SYK-F10       TO   WK-REC
              MOVE       "1"           TO   WK-REC(1198:1)
              MOVE       WK-REC        TO   SND-REC
              WRITE      SND-REC
              ADD        1             TO   WRT-CNT1
              MOVE       SPACE         TO   WK-REC
              MOVE       SYK-F20       TO   WK-REC
              MOVE       "1"           TO   WK-REC(1198:1)
              MOVE       WK-REC        TO   SND-REC
              WRITE      SND-REC
              ADD        1         TO    WRT-CNT1
     END-IF.
*レコード区分毎の処理
*取引レコード区分
     IF      ( TORI-NO   NOT =     SYK-F302 )  OR
             ( LAST-CD   NOT =     SYK-F309 )  OR
             ( LAST-DATE NOT =     SYK-F349 )
               MOVE      SPACE     TO        WK-REC
               MOVE      SYK-F30   TO        WK-REC
*#2018/10/19 NAV ST 訂正後直接納品先納品日に空白セット
               MOVE      SPACE     TO        WK-REC(860:32)
*#2018/10/19 NAV ED 訂正後直接納品先納品日に空白セット
               MOVE      "1"       TO        WK-REC(1198:1)
               MOVE      WK-REC    TO        SND-REC
               WRITE     SND-REC
               ADD       1         TO        WRT-CNT1
               MOVE      SYK-F302  TO        TORI-NO
               MOVE      SYK-F309  TO        LAST-CD
               MOVE      SYK-F349  TO        LAST-DATE
     END-IF.
*取引明細レコード区分
     MOVE      SPACE            TO   WK-REC.
     MOVE      SYK-F40          TO   WK-REC.
     MOVE      "1"              TO   WK-REC(1198:1).
     MOVE      WK-REC    TO        SND-REC.
*
     WRITE     SND-REC.
     ADD       1                   TO        WRT-CNT1.
*出荷メッセージファイル読込
     PERFORM  WKSYKAR1-READ-SEC.
 MAIN-EXIT.
     EXIT.
******************************************************************
*              終了処理                                        *
******************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"           TO   S-NAME.
     MOVE      WRT-CNT1    TO     PARA-KENSUU.
     DISPLAY   "件数＝"  PARA-KENSUU   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*ファイルのクローズ
     CLOSE     WKSYKAR1  SYKSNDF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*
*
******************************************************************
*            出荷メッセージファイル読込
******************************************************************
 WKSYKAR1-READ-SEC           SECTION.
*
     MOVE    "WKSYKAR1-READ-SEC"           TO   S-NAME.
 WKSYKAR1-READ-100.
*
     READ     WKSYKAR1
              AT  END       MOVE  "END"   TO   END-FLG
                            GO     TO     WKSYKAR1-READ-EXIT
              NOT AT  END   ADD    1      TO   RD-CNT1
     END-READ.
*
*件数表示
     IF       RD-CNT1(6:3)   =  "000"  OR  "500"
              DISPLAY "# READ-CNT = " RD-CNT1
              UPON CONS
     END-IF.
*
 WKSYKAR1-READ-EXIT.
     EXIT.

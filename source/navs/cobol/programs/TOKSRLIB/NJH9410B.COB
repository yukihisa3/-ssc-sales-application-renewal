****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　
*    業務名　　　　　　　：　ＥＤＩ　　　　　　　　　　　　　　
*    サブシステム　　　　：　ヨドバシ　ＥＤＩ　　　　　　　
*    モジュール名　　　　：　ヨドバシ発注データチェック結果出力
*    作成日／作成者　　　：　2021/07/26 INOUE
*    処理概要　　　　　　：　チェック結果CSVの
*                            項目タイトル行のみ出力
*    更新日／更新者　　　：　
*    更新内容　　　　　　：　
*                            　　　　　　　　　　　　　　　　　
****************************************************************
*
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           NJH9410B.
 AUTHOR.               NAV-ASSIST.
 DATE-WRITTEN.         2021/07/26.
*
 ENVIRONMENT           DIVISION.
 CONFIGURATION         SECTION.
 SOURCE-COMPUTER.      FUJITSU.
 OBJECT-COMPUTER.      FUJITSU.
 SPECIAL-NAMES.
     CONSOLE IS        CONS.
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*ヨドバシ発注データチェック結果CSV
     SELECT   YODCHKCS  ASSIGN    TO   DA-01-S-YODCHKCS
                        ACCESS MODE    IS    SEQUENTIAL
                        ORGANIZATION   IS    SEQUENTIAL
                        FILE   STATUS  IS    CSV-ST.
*
******************************************************************
*                                                                *
*    DATA              DIVISION                                  *
*                                                                *
******************************************************************
*
 DATA                  DIVISION.
 FILE                  SECTION.
*ヨドバシ発注データチェック結果CSV
 FD  YODCHKCS          BLOCK CONTAINS 11   RECORDS
                       LABEL RECORD   IS   STANDARD.
 01  CSV-REC.
     03 CSV-S01S       PIC X(01).
     03 CSV-F01        PIC N(02).
     03 CSV-S01E       PIC X(01).
     03 CSV-K01        PIC X(01).
*
     03 CSV-S02S       PIC X(01).
     03 CSV-F02        PIC N(04).
     03 CSV-S02E       PIC X(01).
     03 CSV-K02        PIC X(01).
*
     03 CSV-S03S       PIC X(01).
     03 CSV-F03        PIC N(03).
     03 CSV-S03E       PIC X(01).
     03 CSV-K03        PIC X(01).
*
     03 CSV-S04S       PIC X(01).
     03 CSV-F04        PIC N(03).
     03 CSV-S04E       PIC X(01).
     03 CSV-K04        PIC X(01).
*
     03 CSV-S05S       PIC X(01).
     03 CSV-F05        PIC N(03).
     03 CSV-S05E       PIC X(01).
     03 CSV-K05        PIC X(01).
*
     03 CSV-S06S       PIC X(01).
     03 CSV-F06        PIC N(03).
     03 CSV-S06E       PIC X(01).
     03 CSV-K06        PIC X(01).
*
     03 CSV-S07S       PIC X(01).
     03 CSV-F07        PIC N(03).
     03 CSV-S07E       PIC X(01).
     03 CSV-K07        PIC X(01).
*
     03 CSV-S08S       PIC X(01).
     03 CSV-F08        PIC N(04).
     03 CSV-S08E       PIC X(01).
     03 CSV-K08        PIC X(01).
*
     03 CSV-S09S       PIC X(01).
     03 CSV-F09        PIC N(05).
     03 CSV-S09E       PIC X(01).
     03 CSV-K09        PIC X(01).
*
     03 CSV-S10S       PIC X(01).
     03 CSV-F10        PIC N(02).
     03 CSV-S10E       PIC X(01).
     03 CSV-K10        PIC X(01).
*
     03 CSV-S11S       PIC X(01).
     03 CSV-F11        PIC N(07).
     03 CSV-S11E       PIC X(01).
     03 CSV-K11        PIC X(01).
*
     03 CSV-S12S       PIC X(01).
     03 CSV-F12        PIC N(04).
     03 CSV-S12E       PIC X(01).
     03 CSV-K12        PIC X(01).
*
     03 CSV-S13S       PIC X(01).
     03 CSV-F13        PIC N(02).
     03 CSV-S13E       PIC X(01).
     03 CSV-K13        PIC X(01).
*
     03 CSV-S14S       PIC X(01).
     03 CSV-F14        PIC N(04).
     03 CSV-S14E       PIC X(01).
     03 CSV-K14        PIC X(01).
*
     03 CSV-S15S       PIC X(01).
     03 CSV-F15        PIC N(05).
     03 CSV-S15E       PIC X(01).
     03 CSV-K15        PIC X(01).
*
     03 CSV-S16S       PIC X(01).
     03 CSV-F16        PIC N(05).
     03 CSV-S16E       PIC X(01).
     03 CSV-K16        PIC X(01).
*
     03 CSV-S17S       PIC X(01).
     03 CSV-F17        PIC N(05).
     03 CSV-S17E       PIC X(01).
     03 CSV-K17        PIC X(01).
*
     03 CSV-S18S       PIC X(01).
     03 CSV-F18        PIC N(06).
     03 CSV-S18E       PIC X(01).
     03 CSV-K18        PIC X(01).
*
     03 CSV-S19S       PIC X(01).
     03 CSV-F19        PIC N(05).
     03 CSV-S19E       PIC X(01).
     03 CSV-K19        PIC X(01).
*
     03 CSV-S20S       PIC X(01).
     03 CSV-F20        PIC N(02).
     03 CSV-S20E       PIC X(01).
     03 CSV-K20        PIC X(01).
*
     03 CSV-S21S       PIC X(01).
     03 CSV-F21        PIC N(02).
     03 CSV-S21E       PIC X(01).
     03 CSV-K21        PIC X(01).
*
     03 CSV-S22S       PIC X(01).
     03 CSV-F22        PIC N(06).
     03 CSV-S22E       PIC X(01).
     03 CSV-K22        PIC X(01).
*
     03 CSV-S23S       PIC X(01).
     03 CSV-F23        PIC N(03).
     03 CSV-S23E       PIC X(01).
     03 CSV-K23        PIC X(01).
*
     03 CSV-S24S       PIC X(01).
     03 CSV-F24        PIC N(01).
     03 CSV-S24E       PIC X(01).
     03 CSV-K24        PIC X(01).
*
     03 CSV-S25S       PIC X(01).
     03 CSV-F25        PIC N(02).
     03 CSV-S25E       PIC X(01).
     03 CSV-K25        PIC X(01).
*
     03 CSV-S26S       PIC X(01).
     03 CSV-F26        PIC N(02).
     03 CSV-S26E       PIC X(01).
     03 CSV-K26        PIC X(01).
*
     03 CSV-S27S       PIC X(01).
     03 CSV-F27        PIC N(06).
     03 CSV-S27E       PIC X(01).
     03 CSV-K27        PIC X(01).
*
     03 CSV-S28S       PIC X(01).
     03 CSV-F28        PIC N(03).
     03 CSV-S28E       PIC X(01).
     03 CSV-K28        PIC X(01).
*
     03 CSV-S29S       PIC X(01).
     03 CSV-F29        PIC N(05).
     03 CSV-S29E       PIC X(01).
     03 CSV-K29        PIC X(01).
*
     03 CSV-S30S       PIC X(01).
     03 CSV-F30        PIC N(06).
     03 CSV-S30E       PIC X(01).
     03 CSV-K30        PIC X(01).
*
     03 CSV-S31S       PIC X(01).
     03 CSV-F31        PIC N(03).
     03 CSV-S31E       PIC X(01).
     03 CSV-K31        PIC X(01).
*
     03 CSV-S32S       PIC X(01).
     03 CSV-F32        PIC N(02).
     03 CSV-S32E       PIC X(01).
     03 CSV-K32        PIC X(01).
*
     03 CSV-S33S       PIC X(01).
     03 CSV-F33        PIC N(01).
     03 CSV-S33E       PIC X(01).
     03 CSV-K33        PIC X(01).
*
     03 CSV-S34S       PIC X(01).
     03 CSV-F34        PIC N(01).
     03 CSV-S34E       PIC X(01).
     03 CSV-K34        PIC X(01).
*
     03 CSV-S35S       PIC X(01).
     03 CSV-F35        PIC N(01).
     03 CSV-S35E       PIC X(01).
     03 CSV-K35        PIC X(01).
*
     03 CSV-S36S       PIC X(01).
     03 CSV-F36        PIC N(01).
     03 CSV-S36E       PIC X(01).
     03 CSV-K36        PIC X(01).
*
     03 FILLER         PIC X(02).
*
******************************************************************
*                 WORKING-STORAGE   SECTION
******************************************************************
 WORKING-STORAGE       SECTION.
*
 77  CSV-ST                PIC  X(02)     VALUE  "00".
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE           PIC   9(06)  VALUE  ZERO.
     03  SYS-DATEW          PIC   9(08)  VALUE  ZERO.
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME            PIC   9(08)  VALUE  ZERO.
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
     03  LINK-OUT2-RET      PIC   X(01).
     03  LINK-OUT2-YMD8     PIC   9(08).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NJH9410B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH9410B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH9410B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN-ORD.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " ORDERS= ".
         05  IN-CNT-ORD     PIC   9(07).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-IN-CHG.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " ORDCHG= ".
         05  IN-CNT-CHG     PIC   9(07).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE "    OUT= ".
         05  OUT-CNT        PIC   9(07).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
*LINKAGE SECTION.
*  01  LINK-IN-BDATE              PIC 9(08).
*  01  LINK-IN-BTIME              PIC 9(04).
*  01  LINK-IN-BUMCD              PIC X(04).
*  01  LINK-IN-TANCD              PIC X(02).
*  01  LINK-IN-TORCD              PIC 9(08).
*  01  LINK-OUT-KENSU             PIC 9(07).
*  01  LINK-OUT-KEKKA             PIC X(02).
******************************************************************
*                                                                *
*    PROCEDURE         DIVISION                                  *
*                                                                *
******************************************************************
*
 PROCEDURE             DIVISION.
*PROCEDURE             DIVISION   USING
*                                        LINK-IN-BDATE
*                                        LINK-IN-BTIME
*                                        LINK-IN-BUMCD
*                                        LINK-IN-TANCD
*                                        LINK-IN-TORCD
*                                        LINK-OUT-KENSU
*                                        LINK-OUT-KEKKA.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   YODCHKCS.
     MOVE      "YODCHKCS"   TO   AB-FILE.
     MOVE      CSV-ST       TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
*    MOVE     "ER"          TO   LINK-OUT-KEKKA.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
 PRG-CONTROL  SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM      INIT-SEC.
     PERFORM      MAIN-SEC.
     PERFORM      END-SEC.
     STOP RUN.
*
*----------------------------------------------------------------*
*       LEVEL     1    ｲﾆｼｬﾙ ｼｮﾘ                                 *
*----------------------------------------------------------------*
*
 INIT-SEC  SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     DISPLAY  MSG-START UPON CONS.
*
 INIT-0.
     OPEN    OUTPUT  YODCHKCS.
*
 INIT-1.
*   システム日付編集*
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*   システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*
 INIT-EXT.
     EXIT.
*
*----------------------------------------------------------------*
*       LEVEL     1    ﾒｲﾝ   ｼｮﾘ  ORDERS                         *
*----------------------------------------------------------------*
*
 MAIN-SEC   SECTION.
     MOVE  "MAIN-SEC"             TO   S-NAME.
*
 MAIN-ORD-01.
*
     MOVE   SPACE                 TO   CSV-REC.
     MOVE   NC"種別"              TO   CSV-F01.
     MOVE   NC"発注番号"          TO   CSV-F02.
     MOVE   NC"発注日"            TO   CSV-F03.
     MOVE   NC"仕入先"            TO   CSV-F04.
     MOVE   NC"発注先"            TO   CSV-F05.
     MOVE   NC"出荷先"            TO   CSV-F06.
     MOVE   NC"明細行"            TO   CSV-F07.
     MOVE   NC"品目番号"          TO   CSV-F08.
     MOVE   NC"キャンセル"        TO   CSV-F09.
     MOVE   NC"単価"              TO   CSV-F10.
     MOVE   NC"お客様伝票番号"    TO   CSV-F11.
     MOVE   NC"配送場所"          TO   CSV-F12.
     MOVE   NC"数量"              TO   CSV-F13.
     MOVE   NC"納入期日"          TO   CSV-F14.
     MOVE   NC"バッチ＿日"        TO   CSV-F15.
     MOVE   NC"バッチ＿時"        TO   CSV-F16.
     MOVE   NC"取引先ＣＤ"        TO   CSV-F17.
     MOVE   NC"基幹伝票番号"      TO   CSV-F18.
     MOVE   NC"基幹行番号"        TO   CSV-F19.
     MOVE   NC"部門"              TO   CSV-F20.
     MOVE   NC"担当"              TO   CSV-F21.
     MOVE   NC"エラー（種別"      TO   CSV-F22.
     MOVE   NC"発注日"            TO   CSV-F23.
     MOVE   NC"行"                TO   CSV-F24.
     MOVE   NC"数量"              TO   CSV-F25.
     MOVE   NC"納期"              TO   CSV-F26.
     MOVE   NC"同一伝票あり"      TO   CSV-F27.
     MOVE   NC"店舗Ｍ"            TO   CSV-F28.
     MOVE   NC"変換ＴＢＬ"        TO   CSV-F29.
     MOVE   NC"該当伝票なし"      TO   CSV-F30.
     MOVE   NC"計上済"            TO   CSV-F31.
     MOVE   NC"単価"              TO   CSV-F32.
     MOVE   NC"　"                TO   CSV-F33.
     MOVE   NC"　"                TO   CSV-F34.
     MOVE   NC"　"                TO   CSV-F35.
     MOVE   NC"）"                TO   CSV-F36.
*
     MOVE        ","              TO   CSV-K01 CSV-K02 CSV-K03
                                       CSV-K04 CSV-K05 CSV-K06
                                       CSV-K07 CSV-K08 CSV-K09
                                       CSV-K10 CSV-K11 CSV-K12
                                       CSV-K13 CSV-K14 CSV-K15
                                       CSV-K16 CSV-K17 CSV-K18
                                       CSV-K19 CSV-K20 CSV-K21
                                       CSV-K22 CSV-K23 CSV-K24
                                       CSV-K25 CSV-K26 CSV-K27
                                       CSV-K28 CSV-K29 CSV-K30
                                       CSV-K31 CSV-K32 CSV-K33
                                       CSV-K34 CSV-K35 CSV-K36.
     MOVE       X"28"             TO   CSV-S01S CSV-S02S CSV-S03S
                                       CSV-S04S CSV-S05S CSV-S06S
                                       CSV-S07S CSV-S08S CSV-S09S
                                       CSV-S10S CSV-S11S CSV-S12S
                                       CSV-S13S CSV-S14S CSV-S15S
                                       CSV-S16S CSV-S17S CSV-S18S
                                       CSV-S19S CSV-S20S CSV-S21S
                                       CSV-S22S CSV-S23S CSV-S24S
                                       CSV-S25S CSV-S26S CSV-S27S
                                       CSV-S28S CSV-S29S CSV-S30S
                                       CSV-S31S CSV-S32S CSV-S33S
                                       CSV-S34S CSV-S35S CSV-S36S.
     MOVE       X"29"             TO   CSV-S01E CSV-S02E CSV-S03E
                                       CSV-S04E CSV-S05E CSV-S06E
                                       CSV-S07E CSV-S08E CSV-S09E
                                       CSV-S10E CSV-S11E CSV-S12E
                                       CSV-S13E CSV-S14E CSV-S15E
                                       CSV-S16E CSV-S17E CSV-S18E
                                       CSV-S19E CSV-S20E CSV-S21E
                                       CSV-S22E CSV-S23E CSV-S24E
                                       CSV-S25E CSV-S26E CSV-S27E
                                       CSV-S28E CSV-S29E CSV-S30E
                                       CSV-S31E CSV-S32E CSV-S33E
                                       CSV-S34E CSV-S35E CSV-S36E.
*
     WRITE     CSV-REC.
*
*
 MAIN-EXT.
     EXIT.
*
*----------------------------------------------------------------*
*       LEVEL     1    ｴﾝﾄﾞ  ｼｮﾘ                                 *
*----------------------------------------------------------------*
*
 END-SEC  SECTION.
     MOVE    "END-SEC"          TO   S-NAME.
     CLOSE   YODCHKCS.
*
     DISPLAY MSG-END       UPON CONS.
**
 END-EXT.
     EXIT.
 END PROGRAM NJH9410B.
